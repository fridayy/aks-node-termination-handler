%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aksnth_action_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_action/2, start_configured_actions/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_action(Module, Event) -> supervisor:startchild_ret() | {ok, ignored} when
    Module :: module(),
    Event :: map().
start_action(Module, Event) ->
    case Module:enabled() of
        true ->
            supervisor:start_child(?MODULE, [Module, Event]);
        false ->
            logger:info("Skipping action '~p' as it is not enabled", [Module]),
            {ok, ignored}
    end.

start_configured_actions(Event) ->
    case aksnth_config:get_env(actions, []) of
        [] ->
            logger:warning("No actions configured - skipping event"),
            [];
        Modules ->
            lists:filtermap(
                fun(ActionModule) ->
                    case start_action(ActionModule, Event) of
                        {ok, Pid} when is_pid(Pid) ->
                            {true, Pid};
                        {ok, ignored} ->
                            false
                    end
                end,
                Modules
            )
    end.

init(_) ->
    ChildSpec = [
        #{
            id => aksnth_action,
            start => {aksnth_action, start_link, []},
            restart => transient,
            type => worker
        }
    ],

    {ok, {
        #{
            strategy => simple_one_for_one,
            intensity => 5,
            period => 30
        },
        ChildSpec
    }}.
