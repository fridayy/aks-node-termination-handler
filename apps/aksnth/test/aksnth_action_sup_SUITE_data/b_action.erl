%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2022 12:32 PM
%%%-------------------------------------------------------------------
-module(b_action).
-author("bnjm").


-behavior(aksnth_action).

%% API
-export([process/1, enabled/0]).

process(#{ref := Ref, reply_to := Pid}) ->
  Pid ! {ok, Ref, self(), ?MODULE},
  ok.


enabled() -> true.