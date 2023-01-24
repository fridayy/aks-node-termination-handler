%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 7:12 PM
%%%-------------------------------------------------------------------
-module(aksnth_route_health).
-author("bnjm").

%% API
-export([init/2]).

init(#{method := <<"GET">>} = Req, State) ->
    Res = cowboy_req:reply(
        200,
        #{<<"Content-Type">> => <<"application/json">>},
        jsone:encode(#{health => <<"UP">>}),
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
