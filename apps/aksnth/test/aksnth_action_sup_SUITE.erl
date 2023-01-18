%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2022 9:51 AM
%%%-------------------------------------------------------------------
-module(aksnth_action_sup_SUITE).
-author("bnjm").
-compile([export_all]).
-compile(nowarn_export_all).

%% fixtures
all() -> ct_help:all(?MODULE).

init_per_suite(Config) ->
    ct_help:compile_all_data_dir(Config),
    Config.

end_per_suite(_) -> ok.

%% tests

processes_an_event_and_shutdown(_) ->
    process_flag(trap_exit, true),
    {ok, SupPid} = aksnth_action_sup:start_link(),
    Ref = make_ref(),
    {ok, ChildPid} = aksnth_action_sup:start_action(replying_action, #{
        reply_to => self(), ref => Ref
    }),
    ct:sleep(100),
    ok = do_assert_receive({ok, Ref, ChildPid}),
    false = is_process_alive(ChildPid),
    true = is_process_alive(SupPid).

configured_actions_are_triggered(_) ->
    process_flag(trap_exit, true),
    aksnth_config:put_env(actions, [
        a_action,
        b_action
    ]),
    {ok, SupPid} = aksnth_action_sup:start_link(),
    Ref = make_ref(),
    [PidA, PidB] = aksnth_action_sup:start_configured_actions(#{reply_to => self(), ref => Ref}),
    ct:sleep(100),
    do_assert_receive({ok, Ref, PidA, a_action}),
    do_assert_receive({ok, Ref, PidB, b_action}),
    false = is_process_alive(PidA),
    false = is_process_alive(PidB),
    true = is_process_alive(SupPid).

does_retry_on_failure(_) ->
    process_flag(trap_exit, true),
    {ok, SupPid} = aksnth_action_sup:start_link(),
    Ref = make_ref(),
    {ok, ChildPid} = aksnth_action_sup:start_action(retrying_action, #{
        reply_to => self(), ref => Ref
    }),
    ct:sleep(100),
    ok = do_assert_receive({ok, Ref}),
    false = is_process_alive(ChildPid),
    true = is_process_alive(SupPid).

do_assert_receive(M) ->
    receive
        M -> ok
    after 1000 -> exit("fail")
    end.
