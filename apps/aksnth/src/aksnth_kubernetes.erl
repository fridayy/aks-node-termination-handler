%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Caches the kuberlnetes in_cluster server config
%%% @end
%%% Created : 26. Sep 2022 1:07 PM
%%%-------------------------------------------------------------------
-module(aksnth_kubernetes).
-author("bnjm").

%% API

-export([in_cluster/0, create/2, microtime_now/0, get/2, patch/2]).

-define(TABLE, aksnth_kubernetes_server_cfg).


in_cluster() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [set, public, named_table]),
            ServerConfig = kuberlnetes:in_cluster(),
            ets:insert(?TABLE, {ServerConfig}),
            ServerConfig;
        _ ->
            ets:first(?TABLE)
    end.

get(Path, Opts) ->
    kuberlnetes:get(Path, Opts).

create(Request, Opts) ->
    kuberlnetes:post(Request, Opts).

patch(Request, Opts) ->
    kuberlnetes:patch(Request, Opts).

-spec microtime_now() -> binary().
microtime_now() ->
    kuberlnetes:microtime_now().
