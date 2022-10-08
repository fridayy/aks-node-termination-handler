%%%-------------------------------------------------------------------
%% @doc aksnth public API
%% @end
%%%-------------------------------------------------------------------

-module(aksnth_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:info("Starting aks node termination handler version 0.0.1"),
    log_env(),
    boot_cowboy(),
    aksnth_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
log_env() ->
    logger:info("aksnth environment:"),
    lists:foreach(
        fun({K, V}) ->
            logger:info("~p = ~p", [K, V])
        end,
        aksnth_config:all()
    ).

%% bootstraps cowboy on port 8080 serving health and eviction simulation routes
boot_cowboy() ->
    Routes = cowboy_router:compile([
        {'_', [
            {"/health", aksnth_route_health, []},
            {"/simulate-eviction", aksnth_route_simulate_eviction, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(aksnth_http_listener, [{port, 8080}], #{
        env => #{dispatch => Routes}
    }).
