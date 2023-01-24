%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% Drains the node this aksth instance is running on. Does so by:
%%% 1. tainting the node with NoSchedule
%%% 2. cordoning the node
%%% 3. creating pod eviction events
%%%
%%% atm this steps (and the creation of eviction events) are sequential and blocking
%%% operations.
%%%
%%% @end
%%% Created : 27. Sep 2022 10:57 AM
%%%-------------------------------------------------------------------
-module(aksnth_action_drain_node).
-author("bnjm").

-behavior(aksnth_action).

%% API
-export([process/1, enabled/0]).
-include_lib("kernel/include/logger.hrl").

-define(KUBE_OPTS, #{server => aksnth_kubernetes:in_cluster()}).

enabled() -> true.

process(Event) ->
    NodeName = aksnth_config:get_env(node_name),
    Node = read_node(NodeName),
    ?LOG_DEBUG(#{event => taint_node, node => NodeName, action => ?MODULE}),
    ok = taint(Node, Event),
    ?LOG_INFO(#{event => tainted_node, node => NodeName, action => ?MODULE}),
    ?LOG_DEBUG(#{event => codon_node, node => NodeName, action => ?MODULE}),
    ok = cordon(NodeName),
    ?LOG_INFO(#{event => codoned_node, node => NodeName, action => ?MODULE}),
    ?LOG_DEBUG(#{event => drain_node, node => NodeName, action => ?MODULE}),
    ok = drain(NodeName),
    ?LOG_INFO(#{event => drained_node, node => NodeName, action => ?MODULE}),
    ok.

%% internal functions

read_node(NodeName) when is_list(NodeName) ->
    Path = "/api/v1/nodes/" ++ NodeName,
    aksnth_kubernetes:get(Path, ?KUBE_OPTS).

taint(
    #{<<"spec">> := NodeSpec, <<"metadata">> := #{<<"name">> := NodeName}},
    #{<<"EventType">> := EventType, <<"EventId">> := EventId}
) when is_binary(EventType) ->
    CurrentTaints = maps:get(<<"taints">>, NodeSpec, []),
    TaintToAdd = #{
        <<"key">> => <<"aksnth/", EventType/binary>>,
        <<"value">> => EventId,
        <<"effect">> => <<"NoSchedule">>
    },
    Path = "/api/v1/nodes/" ++ erlang:binary_to_list(NodeName),
    aksnth_kubernetes:patch(
        #{
            path => Path,
            body => #{
                <<"spec">> => #{<<"taints">> => [TaintToAdd | CurrentTaints]}
            }
        },
        ?KUBE_OPTS
    ).

cordon(NodeName) ->
    Path = "/api/v1/nodes/" ++ NodeName,
    aksnth_kubernetes:patch(
        #{
            path => Path,
            body => #{
                <<"spec">> => #{<<"unschedulable">> => true}
            }
        },
        ?KUBE_OPTS
    ).

drain(NodeName) ->
    DeletablePods = get_pod_deletion_candidates(NodeName),
    ?LOG_INFO(#{
        event => identified_pod_deletion_candidates,
        count => length(DeletablePods),
        action => ?MODULE
    }),
    lists:foreach(fun create_eviction_obj/1, DeletablePods),
    ok.

create_eviction_obj(#{<<"metadata">> := #{<<"namespace">> := PodNamespace, <<"name">> := PodName}}) ->
    Eviction = #{
        <<"metadata">> => #{<<"namespace">> => PodNamespace, <<"name">> => PodName},
        <<"kind">> => <<"Eviction">>,
        <<"apiVersion">> => <<"policy/v1beta1">>
    },
    Path = <<"/api/v1/namespaces/", PodNamespace/binary, "/pods/", PodName/binary, "/eviction">>,
    ?LOG_DEBUG(#{event => create_eviction_obj, pod => PodName, namespace => PodNamespace}),
    ok = aksnth_kubernetes:create(
        #{path => erlang:binary_to_list(Path), body => Eviction}, ?KUBE_OPTS
    ),
    ?LOG_INFO(#{event => created_eviction_obj, pod => PodName, namespace => PodNamespace}),
    ok.

get_pod_deletion_candidates(NodeName) ->
    PodsOnThisNode = get_pods_on_this_node(NodeName),
    lists:filter(fun is_deletion_candidate/1, PodsOnThisNode).

get_pods_on_this_node(NodeName) when is_list(NodeName) ->
    Path = "/api/v1/pods?fieldSelector=spec.nodeName=" ++ NodeName,
    #{<<"items">> := PodList} = aksnth_kubernetes:get(Path, ?KUBE_OPTS),
    PodList.

%% check if is part of a daemon set - if so skip this pod
is_deletion_candidate(#{
    <<"metadata">> := #{<<"ownerReferences">> := OwnerReferences, <<"name">> := PodName}
}) when is_list(OwnerReferences) ->
    HasDaemonSetRef = lists:any(
        fun(#{<<"kind">> := Kind}) ->
            Kind =:= <<"DaemonSet">>
        end,
        OwnerReferences
    ),
    if
        HasDaemonSetRef ->
            ?LOG_INFO(#{event => skip_daemonset_pod, pod => PodName, action => ?MODULE});
        true ->
            ignore
    end,
    not HasDaemonSetRef;
%% check if already deleted and tearing down - if so skip this one
is_deletion_candidate(#{<<"metadata">> := #{<<"deletionTimestap">> := _TS}}) ->
    false;
%% anything else should be deleted
is_deletion_candidate(_) ->
    true.
