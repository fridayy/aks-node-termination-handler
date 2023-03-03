%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aksnth_event_poller).

-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    handle_continue/2
]).

-export_type([options/0]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-type options() :: #{
    poll_interval => non_neg_integer()
}.

-record(state, {
    %% the name of this virtual machine as acquired by the metadata service
    instance_name :: binary() | undefined,
    %% the received document incarnation see:
    %% https://learn.microsoft.com/en-us/azure/virtual-machines/linux/scheduled-events#event-properties
    last_incarnation :: non_neg_integer() | undefined,
    %% the configured poll interval - received as option
    poll_interval :: non_neg_integer() | undefined
}).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
    PollInterval = aksnth_config:get_env_ensure(poll_interval, non_neg_integer),
    State = set_poll_interval(PollInterval, #state{}),
    {ok, State, {continue, start}}.

handle_continue(start, #state{poll_interval = PollInterval} = State) ->
    %% todo: maybe move this to init instead of continue as the whole system is dependend
    %% on the event poller anyway so it might as well block the supervision tree setup
    aksnth_metadata:init(),
    ?LOG_INFO(#{
        event => poller_initialize,
        message => "Starting metadata event poller with configured interval",
        interval => PollInterval
    }),
    InstanceName = aksnth_metadata:instance_name(),
    poll(PollInterval),
    ?LOG_INFO(#{event => poller_initialized, vm_name => InstanceName}),
    {noreply, State#state{instance_name = InstanceName}}.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(poll, #state{poll_interval = PollInterval} = State) ->
    case aksnth_metadata:try_events() of
        {ok, Response} ->
            handle_events(Response, State);
        {error, empty_response} ->
            %% as empty responses are common from the azure metadata api
            %% they are handled here instead of "let it crash". The main reason
            %% for this is to decrease reaction time if an actual terminal event is returned
            logger:warning(#{
                event => recv_empty_response,
                message => "Received an empty response from metadata service",
                action => skip
            }),
            poll(PollInterval),
            {noreply, State};
        {error, Reason} ->
            % crash on every other response
            {stop, Reason, State}
    end;
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%% internal functions
poll(Interval) ->
    erlang:send_after(Interval, self(), poll).

set_poll_interval(PollInterval, State) when
    is_integer(PollInterval) andalso PollInterval > 0 andalso is_record(State, state)
->
    State#state{poll_interval = PollInterval}.

is_new_incarnation(Incarnation, #state{last_incarnation = LastIncarnation}) ->
    Incarnation =/= LastIncarnation.

events_concerning_this_vm(Events, #state{instance_name = Name}) ->
    lists:filter(
        fun(#{<<"Resources">> := R}) ->
            lists:any(fun(N) -> N =:= Name end, R)
        end,
        Events
    ).

handle_events(
    #{<<"DocumentIncarnation">> := Incarnation, <<"Events">> := Events} = Ev,
    #state{poll_interval = PollInterval} = State
) ->
    ?LOG_DEBUG(#{event => recv_metadata_event, the_event => Ev}),
    case is_new_incarnation(Incarnation, State) of
        true ->
            case events_concerning_this_vm(Events, State) of
                [] ->
                    %% no events regarding this virtual machine - keep polling
                    poll(PollInterval),
                    {noreply, State#state{last_incarnation = Incarnation}};
                [#{<<"EventType">> := <<"Preempt">>} = Event | _] ->
                    %% there is an preemption event for this vm
                    %% the spot VM is lost
                    ?LOG_WARNING(#{
                        event => recv_eviction_event,
                        message => "Received terminmal event",
                        the_event => Event
                    }),
                    aksnth_action_sup:start_configured_actions(Event),
                    ?LOG_INFO(#{
                        event => starting_post_eviction_event_actions,
                        message => "Starting post eviction event actions"
                    }),
                    {stop, normal, State#state{last_incarnation = Incarnation}};
                [Event | _] ->
                    %% catch all for other event types
                    ?LOG_WARNING(#{
                        event => recv_other_event,
                        message => "Received other (unhandled) event",
                        the_event => Event
                    }),
                    poll(PollInterval),
                    {noreply, State#state{last_incarnation = Incarnation}}
            end;
        false ->
            poll(PollInterval),
            {noreply, State#state{last_incarnation = Incarnation}}
    end.
