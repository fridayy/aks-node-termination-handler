%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aksnth_action).

-behaviour(gen_server).

-export([start_link/2]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    handle_continue/2
]).

-callback process(map()) -> ok.
-callback enabled() -> boolean().

-record(state, {
    %% the action callback module
    module :: module(),
    %% the event to be handled by this action
    event :: map()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Module, Event) ->
    gen_server:start_link(?MODULE, [Module, Event], []).

init([Module, Event]) ->
    {ok,
        #state{
            event = Event,
            module = Module
        },
        {continue, process_event}}.

handle_continue(process_event, #state{module = Module, event = Event} = State) ->
    logger:info("[~p] Processing event: ~p", [Module, Event]),
    Module:process(Event),
    logger:info("[~p] Event processed", [Module]),
    {stop, normal, State}.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
