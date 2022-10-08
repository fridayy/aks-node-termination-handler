%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Sep 2022 7:07 PM
%%%-------------------------------------------------------------------
-module(happy_events_h).
-author("bnjm").

%% API
-export([init/2]).

-define(PT_KEY, {?MODULE, calls}).

init(Req, State) ->
  CallCount = call_count(),
  #{'api-version' := <<"2020-07-01">>} = cowboy_req:match_qs(['api-version'], Req),
  <<"true">> = cowboy_req:header(<<"metadata">>, Req),
  {ok, cowboy_req:reply(200, #{<<"Metadata">> => <<"true">>},
    jsone:encode(response(CallCount)), Req), State}.

response(0) -> #{<<"DocumentIncarnation">> => 0, <<"Events">> => []};
response(1) -> #{<<"DocumentIncarnation">> => 1, <<"Events">> => [
     #{<<"EventId">> => <<"9C3EE5B7-E5B5-4482-A07F-DD5C10DAA97A">>,
            <<"EventStatus">> => <<"Scheduled">>,
            <<"EventType">> => <<"Preempt">>,
            <<"ResourceType">> => <<"VirtualMachine">>,
            <<"Resources">> => [<<"aks-spottest-41185300-vmss_2">>],
            <<"NotBefore">> => <<"Mon, 19 Sep 2022 08:01:53 GMT">>,
            <<"Description">> => <<"">>,
            <<"EventSource">> => <<"Platform">>,
            <<"DurationInSeconds">> => -1
     }
]}.

call_count() ->
  case persistent_term:get(?PT_KEY, 0) of
    0 ->
      persistent_term:put(?PT_KEY, 1),
      0;
    Else -> persistent_term:put(?PT_KEY, Else + 1),
      Else
  end.
