%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 7:12 PM
%%%-------------------------------------------------------------------
-module(aksnth_route_simulate_eviction).
-author("bnjm").

%% API
-export([init/2]).

init(#{method := <<"GET">>} = Req, State) ->
    logger:debug("[GET] /simulate-eviction"),
    aksnth_metadata_mock:simulate_eviction(),
    Res = cowboy_req:reply(
        201,
        #{<<"Content-Type">> => <<"application/json">>},
        Req
    ),
    {ok, Res, State};
init(Req, State) ->
    Res = cowboy_req:reply(
        405,
        #{<<"allow">> => <<"get">>},
        Req
    ),
    {ok, Res, State}.
