%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, leftshift.one software gmbh
%%% @doc
%%% Pushes a metric to a configured prometheus push gateway 
%%% if an eviction event is received.
%%%
%%% example metric:
%%%  spot_instance_eviction_count{
%%%  job = "aks-node-termination-handler", 
%%%  node = "aks-genspotc4m16-29758349-vmss_9",
%%%  instance = "aksnth-aks-node-termination-handler-dms9n"
%%%  } 1
%%%
%%% @end
%%% Created : 2023-01-18 14:15
%%%-------------------------------------------------------------------
-module(aksnth_action_prometheus_push).

-author("bnjm").

-behaviour(aksnth_action).

-include_lib("kernel/include/logger.hrl").

%%callbacks
-export([enabled/0, process/1]).

enabled() ->
  aksnth_config:is_defined(prometheus_pushgateway_url).

process(_) ->
  Job = <<"aks-node-termination-handler">>,
  Node = aksnth_config:get_env_ensure(node_name, binary),
  Instance = aksnth_config:get_env_ensure(pod_name, binary),
  PushGatewayUrl = aksnth_config:get_env_ensure(prometheus_pushgateway_url, string),
  CompleteUri = io_lib:format("~s/job/~s/instance/~s", [PushGatewayUrl, 
                                                        Job,
                                                        Instance
                                                       ]),
  
  ?LOG_INFO(#{action => ?MODULE, url => CompleteUri, node => Node}),
  Content = <<"# TYPE spot_instance_eviction_count counter\n",
              "spot_instance_eviction_count{node=", $", Node/binary, $", "} 1\n"
            >>,
  %% post to pushgateway - just crash on failure
  {ok, {{_, 200, _}, _, _}} = httpc:request(
        post,
        {CompleteUri, [], "application/x-www-form-urlencoded", Content},
        [],
        []
    ),
  ?LOG_INFO(#{action => ?MODULE, outcome => success}),
  ok.

