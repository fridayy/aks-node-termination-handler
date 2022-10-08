%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Sep 2022 12:04 AM
%%%-------------------------------------------------------------------
-module(aksnth_metadata_SUITE).
-author("bnjm").
-compile([export_all]).
-compile(nowarn_export_all).

%% fixtures
all() ->
    [{group, happy}].

groups() ->
    [
        {happy, [parallel], [
            returns_expected_instance_data,
            returns_expected_events
        ]}
    ].

init_per_suite(Config) ->
    ct_help:compile_all_data_dir(Config),
    Config.

end_per_suite(_) -> ok.

init_per_group(Name, Config) ->
    application:ensure_all_started(cowboy),
    {ok, _} = cowboy:start_clear(Name, [{port, 0}], #{env => #{dispatch => routes(Name)}}),
    Port = ranch:get_port(Name),
    aksnth_config:put_env(
        azure_metadata_service_url, "http://localhost:" ++ erlang:integer_to_list(Port)
    ),
    Config.

routes(happy) ->
    cowboy_router:compile([
        {"localhost", [
            {"/metadata/instance", happy_instance_h, []},
            {"/metadata/scheduledevents", happy_events_h, []}
        ]}
    ]).

end_per_group(Name, _) ->
    cowboy:stop_listener(Name).

%% tests
returns_expected_instance_data(_) ->
    {ok, #{<<"compute">> := #{<<"name">> := <<"aks-spottest-41185300-vmss_2">>}}} = aksnth_metadata:try_instance_information().

returns_expected_events(_) ->
    {ok, #{<<"DocumentIncarnation">> := 0, <<"Events">> := []}} = aksnth_metadata:try_events(),
    {ok, #{
        <<"DocumentIncarnation">> := 1,
        <<"Events">> := [#{<<"EventId">> := <<"9C3EE5B7-E5B5-4482-A07F-DD5C10DAA97A">>}]
    }} = aksnth_metadata:try_events().
