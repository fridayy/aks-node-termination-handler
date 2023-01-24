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

-include_lib("kernel/include/logger.hrl").

-define(TABLE, aksnth_kubernetes_server_cfg).

in_cluster() ->
    case aksnth_config:get_env(metadata_module) of
        aksnth_metadata_mock ->
            ?LOG_WARNING(#{
                event => skip_kubernetes_module,
                message => "Using mock mode - kubernetes module is non functional"
            }),
            ok;
        _ ->
            use_cached_server_config()
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

%% internal
use_cached_server_config() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [set, public, named_table]),
            ServerConfig = kuberlnetes:in_cluster(),
            ets:insert(?TABLE, {ServerConfig}),
            ServerConfig;
        _ ->
            ets:first(?TABLE)
    end.
