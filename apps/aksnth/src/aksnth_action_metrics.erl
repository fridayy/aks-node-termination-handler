%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2023, leftshift.one software gmbh
%%% @doc
%%% Sets the exported observability metrics.
%%% @end
%%% Created : 24. Jan 2023
%%%-------------------------------------------------------------------
-module(aksnth_action_metrics).

-author("bnjm").

-behaviour(aksnth_action).

-export([process/1, enabled/0]).

-include_lib("kernel/include/logger.hrl").

enabled() ->
    true.

process(_) ->
    ?LOG_DEBUG(#{event => seting_metrics}),
    aksnth_metrics:set_spot_node_unavailable(),
    ?LOG_DEBUG(#{event => set_metrics}),
    ok.
