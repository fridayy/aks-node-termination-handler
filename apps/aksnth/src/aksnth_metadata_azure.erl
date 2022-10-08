%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 2:00 PM
%%%-------------------------------------------------------------------
-module(aksnth_metadata_azure).
-author("bnjm").

-behavior(aksnth_metadata).

%% API
-export([try_instance_information/0, instance_name/0, try_events/0]).

-define(API_VERSION_INSTANCE, "2021-02-01").
-define(API_VERSION_EVENTS, "2020-07-01").
-define(HTTP_OPTS, [{timeout, 3000}, {connect_timeout, 3000}]).
-define(HTTP_HEADER, [{"Metadata", "true"}]).

-spec try_instance_information() -> {ok, aksnth_metadata:info_spec()} | {error, invalid_response}.
try_instance_information() ->
    Url = base_url() ++ "/metadata/instance?api-version=" ++ ?API_VERSION_INSTANCE,
    do_get(Url).

-spec instance_name() -> binary().
instance_name() ->
    {ok, #{<<"compute">> := #{<<"name">> := Name}}} = try_instance_information(),
    Name.

-spec try_events() -> {ok, azure_metadata:event_spec()} | {error, invalid_response}.
try_events() ->
    Url = base_url() ++ "/metadata/scheduledevents?api-version=" ++ ?API_VERSION_EVENTS,
    do_get(Url).

%% internal functions
do_get(Url) ->
    case httpc:request(get, {Url, ?HTTP_HEADER}, ?HTTP_OPTS, []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, decode(Body)};
        Else ->
            logger:error("Metadata service unexpectedly responded with: ~p", [Else]),
            {error, invalid_response}
    end.

base_url() ->
    aksnth_config:get_env(azure_metadata_service_url).

%% allow ctrl chars as responses may contain unescaped new line delimiters
decode(X) when is_binary(X) -> jsone:decode(X, [{allow_ctrl_chars, true}]);
decode(X) when is_list(X) -> decode(erlang:list_to_binary(X)).
