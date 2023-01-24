%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2023, leftshift.one software gmbh
%%% @doc
%%% Observability facade
%%% @end
%%% Created : 23. Jan 2023
%%%-------------------------------------------------------------------
-module(aksnth_metrics).

-author("bnjm").

-export([init/0, export/0, 
         set_spot_node_available/0,
         set_spot_node_unavailable/0
        ]).

%% @doc
%% Initializes all available metrics.
%% @end
-spec init() -> ok.
init() ->
  prometheus_gauge:new([{name, aksnth_spot_node_available}, 
                        {help, "Indicates the spot node availability"}, 
                        {labels, [node]}
                       ]),
  prometheus_gauge:new([{name, aksnth_spot_node_eviction_imminent},
                        {help, "Set to 1 if eviction is imminent"},
                        {labels, [node]}
                       ]),
  set_spot_node_available(),
  ok.

set_spot_node_available() -> 
  gauge(aksnth_spot_node_available, 1),
  gauge(aksnth_spot_node_eviction_imminent, 0),
  ok.

set_spot_node_unavailable() -> 
  gauge(aksnth_spot_node_available, 0), 
  gauge(aksnth_spot_node_eviction_imminent, 1).

-spec export() -> binary().
export() ->
  prometheus_text_format:format().

%% internal
gauge(Name, Value) ->
  NodeLabel = aksnth_config:get_env_ensure(node_name, string),
  prometheus_gauge:set(Name, [NodeLabel], Value).
