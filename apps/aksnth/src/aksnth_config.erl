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
-export([get_env/2, get_env/1, put_env/2, get_env_ensure/2, all/0, is_defined/1]).

-type options() ::
    actions
    | node_name
    | pod_name
    | namespace
    | poll_interval
    | metadata_module
    | azure_metadata_service_url.

-spec get_env(Key, Default) -> term() | Default when
    Key :: options(),
    Default :: term().
get_env(Key, Default) ->
    case application:get_env(aksnth, Key) of
        undefined -> Default;
        %% an empty string is considered as an absence
        {ok, []} -> Default;
        {ok, Result} -> Result
    end.

-spec get_env(Key) -> term() when
    Key :: options().
get_env(Key) ->
    case get_env(Key, undefined) of
        undefined -> error({not_set, Key});
        Else -> Else
    end.

%% @doc
%% Ensures the returned result is transformed into the given datatype.
%% @end
-spec get_env_ensure(Key, DataType) -> Result when
    Key :: options(),
    DataType :: non_neg_integer | integer | string | atom | binary,
    Result :: non_neg_integer() | integer() | string() | atom() | binary() | undefined.
get_env_ensure(Key, DataType) -> do_ensure(get_env(Key), DataType).

do_ensure(Value, integer) when is_list(Value) -> erlang:list_to_integer(Value);
do_ensure(Value, integer) when is_integer(Value) -> Value;
do_ensure(Value, non_neg_integer) when is_list(Value) ->
    do_ensure(erlang:list_to_integer(Value), non_neg_integer);
do_ensure(Value, non_neg_integer) when is_integer(Value) andalso Value >= 0 -> Value;
do_ensure(Value, binary) when is_list(Value) -> erlang:list_to_binary(Value);
do_ensure(Value, string) when is_list(Value) -> Value;
do_ensure(Value, string) when is_binary(Value) -> erlang:binary_to_list(Value).

%% should be used with care / in tests only
put_env(Key, Value) ->
    application:set_env(aksnth, Key, Value).

all() -> application:get_all_env(aksnth).

-spec is_defined(Key) -> Result when
    Key :: options(),
    Result :: boolean().
is_defined(Key) ->
    case get_env(Key, undefined) of
        undefined -> false;
        _ -> true
    end.

%% internal
