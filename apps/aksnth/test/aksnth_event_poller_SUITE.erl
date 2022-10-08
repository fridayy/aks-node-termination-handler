%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Sep 2022 9:00 PM
%%%-------------------------------------------------------------------
-module(aksnth_event_poller_SUITE).
-author("bnjm").

-compile([export_all]).
-compile(nowarn_export_all).

all() -> ct_help:all(?MODULE).

init_per_suite(Config) ->
    aksnth_config:put_env(poll_interval, 50),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    meck:new(aksnth_metadata, []),
    meck:new(aksnth_action_sup, []),
    meck:expect(aksnth_action_sup, start_configured_actions, fun(_) -> [] end),
    meck:expect(aksnth_metadata, instance_name, fun() -> <<"some-instance">> end),
    meck:expect(aksnth_metadata, init, fun() -> ok end),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(aksnth_metadata),
    meck:unload(aksnth_action_sup),
    ok.

exists_on_normally_terminal_event(_) ->
    meck:expect(aksnth_metadata, try_events, fun() ->
        {ok, #{
            <<"DocumentIncarnation">> => 0,
            <<"Events">> => [
                #{<<"EventId">> => <<"Id">>, <<"Resources">> => [<<"some-instance">>]}
            ]
        }}
    end),
    process_flag(trap_exit, true),
    {ok, Pid} = aksnth_event_poller:start_link(),
    receive
        {'EXIT', Pid, normal} -> ok
    after 2000 ->
        exit("fail")
    end.

keeps_polling_if_events_does_not_concern_this_vm(_) ->
    meck:expect(
        aksnth_metadata,
        try_events,
        0,
        meck:seq([
            meck:exec(fun() ->
                {ok, #{
                    <<"DocumentIncarnation">> => 0,
                    <<"Events">> => [
                        #{<<"EventId">> => <<"Id">>, <<"Resources">> => [<<"not-me">>]}
                    ]
                }}
            end),
            meck:exec(fun() ->
                {ok, #{
                    <<"DocumentIncarnation">> => 1,
                    <<"Events">> => [
                        #{<<"EventId">> => <<"Id">>, <<"Resources">> => [<<"not-me">>]}
                    ]
                }}
            end),
            meck:exec(fun() ->
                {ok, #{
                    <<"DocumentIncarnation">> => 2,
                    <<"Events">> => [
                        #{<<"EventId">> => <<"Id">>, <<"Resources">> => [<<"not-me">>]}
                    ]
                }}
            end)
        ])
    ),
    {ok, Pid} = aksnth_event_poller:start_link(),
    ct:sleep(150),
    Calls = meck:num_calls(aksnth_metadata, try_events, []),
    true = Calls >= 2,
    true = is_process_alive(Pid).
