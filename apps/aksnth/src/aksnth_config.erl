%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Sep 2022 11:34 PM
%%%-------------------------------------------------------------------
-module(aksnth_config).
-author("bnjm").

%% API
-export([get_env/2, get_env/1, put_env/2, get_env_ensure/2, all/0]).

-type options() ::
    node_name
    | pod_name
    | namespace
    | poll_interval
    | azure_metadata_service_url.

-spec get_env(Key, Default) -> term() | Default when
    Key :: options(),
    Default :: term().
get_env(Key, Default) ->
    case application:get_env(aksnth, Key) of
        undefined -> Default;
        {ok, Result} -> Result
    end.

-spec get_env(Key) -> term() when
    Key :: options().
get_env(Key) ->
    case get_env(Key, undefined) of
        undefined -> error(not_set);
        Else -> Else
    end.

%% @doc
%% Ensures the returned result is transformed into the given datatype.
-spec get_env_ensure(Key, DataType) -> Result when
    Key :: atom(),
    DataType :: integer | string | atom | binary,
    Result :: integer() | string() | atom() | binary() | undefined.
get_env_ensure(Key, DataType) -> do_ensure(get_env(Key), DataType).

do_ensure(Value, integer) when is_list(Value) -> erlang:list_to_integer(Value);
do_ensure(Value, integer) when is_integer(Value) -> Value;
do_ensure(Value, binary) when is_list(Value) -> erlang:list_to_binary(Value).

%% should be used with care / in tests only
%% as persistent_term updates are expensive.
put_env(Key, Value) ->
    application:set_env(aksnth, Key, Value).

all() -> application:get_all_env(aksnth).

%% internal
