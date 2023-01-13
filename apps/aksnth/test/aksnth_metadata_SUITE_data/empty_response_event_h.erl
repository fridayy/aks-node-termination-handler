%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, leftshift.one
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2023 4:03 PM
%%%-------------------------------------------------------------------
-module(empty_response_event_h).

-author("bnjm").

-export([init/2]).

init(Req, State) ->
  #{'api-version' := <<"2020-07-01">>} = cowboy_req:match_qs(['api-version'], Req),
  <<"true">> = cowboy_req:header(<<"metadata">>, Req),
  {ok, cowboy_req:reply(200, #{<<"Metadata">> => <<"true">>}, <<>>, Req), State}.

