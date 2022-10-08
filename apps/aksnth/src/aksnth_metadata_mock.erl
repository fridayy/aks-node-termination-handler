%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Reads responses from an ETS table
%%% @end
%%% Created : 05. Oct 2022 1:59 PM
%%%-------------------------------------------------------------------
-module(aksnth_metadata_mock).
-author("bnjm").

-behavior(aksnth_metadata).

-define(ETS, aksnth_metadata_mock_table).
-define(VM_NAME, <<"SOME_VM">>).

%% API
-export([try_instance_information/0, instance_name/0, try_events/0, init/0, simulate_eviction/0]).

try_instance_information() ->
    erlang:error(not_implemented).

instance_name() -> ?VM_NAME.

try_events() ->
    LastKey = ets:last(?ETS),
    {ok, ets:lookup_element(?ETS, LastKey, 2)}.

init() ->
    ?ETS = ets:new(?ETS, [ordered_set, public, named_table]),
    ets:insert(?ETS, {1, #{<<"DocumentIncarnation">> => <<"1">>, <<"Events">> => []}}),
    logger:debug("ETS table with mock responses initialized"),
    ok.

simulate_eviction() ->
    ets:insert(
        ?ETS,
        {2, #{
            <<"DocumentIncarnation">> => 2,
            <<"Events">> => [
                #{
                    <<"EventId">> => <<"562a2895-d01e-45e8-b8ca-585aba5880c0">>,
                    <<"Resources">> => [?VM_NAME],
                    <<"EventType">> => <<"Preempt">>,
                    <<"ResourceType">> => <<"VirtualMachine">>,
                    <<"EventStatus">> => <<"Scheduled">>,
                    <<"NotBefore">> => <<"Mon, 19 Sep 2016 18:29:47 GMT">>,
                    <<"Description">> => <<"">>,
                    <<"EventSource">> => <<"Platform">>,
                    <<"DurationInSeconds">> => -1
                }
            ]
        }}
    ).
