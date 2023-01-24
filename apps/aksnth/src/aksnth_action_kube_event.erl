%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Creates a v1beta1 kubernetes event out of a Azure scheduled event.
%%% @end
%%% Created : 26. Sep 2022 4:17 PM
%%%-------------------------------------------------------------------
-module(aksnth_action_kube_event).
-author("bnjm").

-behavior(aksnth_action).

%% API
-export([process/1, enabled/0]).

-include_lib("kernel/include/logger.hrl").

enabled() -> true.

process(#{<<"EventType">> := EventType, <<"NotBefore">> := NotBefore}) when
    is_binary(EventType) and is_binary(NotBefore)
->
    Server = aksnth_kubernetes:in_cluster(),
    NodeName = erlang:list_to_binary(aksnth_config:get_env(node_name)),
    ?LOG_DEBUG(#{event => creating_kube_event, node => NodeName, action => ?MODULE}),
    ok = aksnth_kubernetes:create(
        #{
            path => "/apis/events.k8s.io/v1beta1/namespaces/default/events",
            body => #{
                <<"type">> => <<"Warning">>,
                <<"action">> => <<"None">>,
                <<"reason">> => <<"Unhealthy">>,
                <<"note">> =>
                    <<"Node '", NodeName/binary, "' will change into status '", EventType/binary,
                        "' [Not before: '", NotBefore/binary, "']">>,
                <<"eventTime">> => aksnth_kubernetes:microtime_now(),
                <<"reportingController">> => <<"aks-node-termination-handler">>,
                <<"reportingInstance">> => <<"aksnth">>,
                <<"metadata">> => #{<<"generateName">> => <<"aksnth">>},
                <<"regarding">> => #{
                    <<"kind">> => <<"Node">>,
                    <<"name">> => NodeName,
                    <<"namespace">> => erlang:list_to_binary(aksnth_config:get_env(namespace))
                }
            }
        },
        #{server => Server}
    ),
    ?LOG_INFO(#{event => created_kube_event, node => NodeName, action => ?MODULE}),
    ok.
