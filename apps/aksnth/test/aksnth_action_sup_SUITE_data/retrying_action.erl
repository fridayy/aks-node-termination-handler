%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2022 12:40 PM
%%%-------------------------------------------------------------------
-module(retrying_action).
-author("bnjm").

-behavior(aksnth_action).

-define(PT_KEY, {?MODULE, calls}).

%% API
-export([process/1, enabled/0]).



process(#{ref := Ref, reply_to := Pid}) ->
  CallCount = call_count(),
  case CallCount of
    0 -> 1 / 0; %% crash on purpose
    _ -> Pid ! {ok, Ref}
  end,
  ok.

call_count() ->
  case persistent_term:get(?PT_KEY, 0) of
    0 ->
      persistent_term:put(?PT_KEY, 1),
      0;
    Else -> persistent_term:put(?PT_KEY, Else + 1),
      Else
  end.


enabled() -> true.