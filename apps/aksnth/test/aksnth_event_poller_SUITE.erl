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

-define(THIS_VM_NAME, <<"some-instance">>).

all() -> ct_help:all(?MODULE).

init_per_suite(Config) ->
    aksnth_config:put_env(poll_interval, 50),
    aksnth_config:put_env(azure_metadata_service_url, "http://localhost"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    meck:new(aksnth_metadata, []),
    meck:new(aksnth_action_sup, []),
    meck:expect(aksnth_action_sup, start_configured_actions, fun(_) -> [] end),
    meck:expect(aksnth_metadata, instance_name, fun() -> ?THIS_VM_NAME end),
    meck:expect(aksnth_metadata, init, fun() -> ok end),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(aksnth_metadata),
    meck:unload(aksnth_action_sup),
    ok.

exits_normally_on_terminal_event(_) ->
    meck:expect(aksnth_metadata, try_events, fun() -> do_preempt_event_concering_this_vm() end),
    process_flag(trap_exit, true),
    {ok, Pid} = aksnth_event_poller:start_link(),
    receive
        {'EXIT', Pid, normal} -> ok
    after 2000 ->
        exit("fail")
    end.

exits_does_ignore_empty_responses(_) ->
    meck:expect(
        aksnth_metadata,
        try_events,
        0,
        meck:seq([
            meck:exec(fun() -> {error, empty_response} end),
            meck:exec(fun() -> do_preempt_event_concering_this_vm() end)
        ])
    ),
    do_start_and_assert_shutdown(normal).

keeps_polling_if_events_does_not_concern_this_vm(_) ->
    meck:expect(
        aksnth_metadata,
        try_events,
        0,
        meck:seq([
            meck:exec(fun() ->
                do_preempt_event_for(0, <<"not-me">>)
            end),
            meck:exec(fun() ->
                do_preempt_event_for(1, <<"not-me">>)
            end),
            meck:exec(fun() ->
                do_preempt_event_for(2, <<"not-me">>)
            end)
        ])
    ),
    {ok, Pid} = aksnth_event_poller:start_link(),
    ct:sleep(150),
    Calls = meck:num_calls(aksnth_metadata, try_events, []),
    true = Calls >= 2,
    true = is_process_alive(Pid).

keeps_polling_if_event_is_not_preempt(_) ->
    meck:expect(
        aksnth_metadata,
        try_events,
        0,
        meck:seq([
            meck:exec(fun() ->
                do_defined_event_for_this_vm(0, <<"Freeze">>)
            end),
            meck:exec(fun() ->
                do_defined_event_for_this_vm(1, <<"Freeze">>)
            end),
            meck:exec(fun() ->
                do_defined_event_for_this_vm(2, <<"Freeze">>)
            end)
        ])
    ),
    {ok, Pid} = aksnth_event_poller:start_link(),
    ct:sleep(150),
    Calls = meck:num_calls(aksnth_metadata, try_events, []),
    true = Calls >= 2,
    true = is_process_alive(Pid).

%% help fns
do_preempt_event_concering_this_vm() ->
    {ok, #{
        <<"DocumentIncarnation">> => 0,
        <<"Events">> => [
            #{
                <<"EventId">> => <<"Id">>,
                <<"EventType">> => <<"Preempt">>,
                <<"Resources">> => [?THIS_VM_NAME]
            }
        ]
    }}.

do_preempt_event_for(Incarnation, Name) when is_integer(Incarnation) andalso is_binary(Name) ->
    {ok, #{
        <<"DocumentIncarnation">> => Incarnation,
        <<"Events">> => [
            #{
                <<"EventId">> => <<"Id">>,
                <<"EventType">> => <<"Preempt">>,
                <<"Resources">> => [Name]
            }
        ]
    }}.

do_defined_event_for_this_vm(Incarnation, EventType) when
    is_integer(Incarnation) andalso is_binary(EventType)
->
    {ok, #{
        <<"DocumentIncarnation">> => Incarnation,
        <<"Events">> => [
            #{
                <<"EventId">> => <<"Id">>,
                <<"EventType">> => EventType,
                <<"Resources">> => [?THIS_VM_NAME]
            }
        ]
    }}.

do_start_and_assert_shutdown(Reason) ->
    process_flag(trap_exit, true),
    {ok, Pid} = aksnth_event_poller:start_link(),
    receive
        {'EXIT', Pid, Reason} -> ok;
        Else -> exit({fail, Else})
    after 2000 ->
        exit("fail")
    end.
