%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2022 9:51 AM
%%%-------------------------------------------------------------------
-module(aksnth_action_prometheus_push_SUITE).
-author("bnjm").
-compile([export_all]).
-compile(nowarn_export_all).

%% fixtures
%% fixtures
all() ->
    [{group, happy}].

groups() ->
    [
        {happy, [parallel], [
            executes_expected_request
        ]}
    ].

init_per_suite(Config) ->
    ct_help:compile_all_data_dir(Config),
    Config.

init_per_group(Name, Config) -> 
    application:ensure_all_started(cowboy),
    Routes = cowboy_router:compile([
                                    {"localhost", routes(Name)}
                                   ]),
    {ok, _} = cowboy:start_clear(Name, [{port, 0}], #{env => #{dispatch => Routes}}),
    Port = ranch:get_port(Name),
    aksnth_config:put_env(
      prometheus_pushgateway_url, io_lib:format("http://localhost:~w/metrics", [Port])
    ),
    aksnth_config:put_env(node_name, "some-node"),
    aksnth_config:put_env(pod_name, "some-pod"),
    Config.

routes(happy) ->
  [
   {"/metrics/job/:job/instance/:instance", happy_push_route_h, []}
  ].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name).

end_per_suite(_) -> ok.

%% tests
executes_expected_request(_) ->
  true = aksnth_action_prometheus_push:enabled(),
  ok = aksnth_action_prometheus_push:process(#{}).
  
