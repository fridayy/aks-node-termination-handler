%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2022 2:18 PM
%%%-------------------------------------------------------------------
-module(aksnth_action_webhook).
-author("bnjm").

-behavior(aksnth_action).

%% API
-export([process/1, enabled/0]).

process(_) ->
    erlang:error(not_implemented).

enabled() ->
    case aksnth_config:get_env(webhook_url, undefined) of
        undefined -> false;
        Else -> is_valid_uri(Else)
    end.

%% internal functions
is_valid_uri(Uri) when is_list(Uri) ->
    case uri_string:parse(Uri) of
        {error, _, _} -> false;
        _Ok -> true
    end;
is_valid_uri(_Uri) ->
    false.
