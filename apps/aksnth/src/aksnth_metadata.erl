%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Retrieves instance metadata via the Azure Metadata Service
%%% see: https://learn.microsoft.com/en-us/azure/virtual-machines/windows/instance-metadata-service?tabs=linux
%%% @end
%%% Created : 24. Sep 2022 11:23 PM
%%%-------------------------------------------------------------------
-module(aksnth_metadata).
-author("bnjm").

-callback init() -> term().
-callback try_instance_information() -> {ok, info_spec()} | {error, invalid_response}.
-callback instance_name() -> binary().
-callback try_events() -> {ok, event_spec()} | {error, invalid_response}.

-optional_callbacks([init/0]).

%% API
-export([try_instance_information/0, try_events/0, instance_name/0, init/0]).

%% TODO: proper spec for azure scheduled events
-export_type([info_spec/0, event_spec/0]).

-type info_spec() :: #{}.
-type event_spec() :: #{}.

init() ->
    Mod = impl(),
    try
        erlang:apply(Mod, ?FUNCTION_NAME, [])
    catch
        error:undef ->
            logger:debug("init/0 not exported from '~p'", [Mod]),
            ok;
        _ ->
            logger:error("~p:init/0 exited with an error", [Mod]),
            throw(init_error)
    end.

-spec try_instance_information() -> {ok, info_spec()} | {error, invalid_response}.
try_instance_information() ->
    erlang:apply(impl(), ?FUNCTION_NAME, []).

-spec instance_name() -> binary().
instance_name() ->
    erlang:apply(impl(), ?FUNCTION_NAME, []).

-spec try_events() -> {ok, event_spec()} | {error, invalid_response}.
try_events() ->
    erlang:apply(impl(), ?FUNCTION_NAME, []).

%% internal functions
impl() ->
    aksnth_config:get_env(metadata_module, aksnth_metadata_azure).
