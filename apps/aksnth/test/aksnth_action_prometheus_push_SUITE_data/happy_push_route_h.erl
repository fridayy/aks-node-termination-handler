-module(happy_push_route_h).

-compile([export_all]).

init(#{method := <<"POST">>} = Req0, State) ->
  ExpectedBody = <<"# TYPE spot_instance_eviction_count counter\nspot_instance_eviction_count{node=\"some-node\"} 1\n">>,
  true = cowboy_req:has_body(Req0),
  #{
    job := <<"aks-node-termination-handler">>,
    instance := <<"some-pod">>
   } = cowboy_req:bindings(Req0),
  {ok, ExpectedBody, Req1} = cowboy_req:read_body(Req0),
  Res = cowboy_req:reply(200, Req1),
  {ok, Res, State};

init(Req, State) ->
  Res = cowboy_req:reply(400, Req),
  {ok, Res, State}.
