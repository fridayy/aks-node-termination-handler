%%%-------------------------------------------------------------------
%% @doc aksnth top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(aksnth_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 10
    },
    Children = [
        action_supervisor(),
        event_poller()
    ],
    {ok, {SupFlags, Children}}.

%% internal functions
action_supervisor() ->
    #{
        id => aksnth_action_sup,
        start => {aksnth_action_sup, start_link, []},
        restart => transient,
        type => supervisor
    }.

event_poller() ->
    #{
        id => aksnth_event_poller,
        start => {aksnth_event_poller, start_link, []},
        restart => transient,
        type => worker
    }.
