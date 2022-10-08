%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2022 1:07 PM
%%%-------------------------------------------------------------------
-module(aksnth_kubernetes).
-author("bnjm").

%% API

-export([in_cluster/0, create/2, microtime_now/0, get/2, patch/2]).

-type options() :: #{
    server => server() | undefined
}.

-type mutation_request() :: #{
    path => string(),
    body => map()
}.

-record(auth_token, {token :: binary()}).
-record(server, {
    url :: string(),
    ca_cert :: string(),
    auth :: #auth_token{},
    skip_tls_verify :: boolean()
}).

-type server() :: #server{}.

in_cluster() ->
    Host = os:getenv("KUBERNETES_SERVICE_HOST"),
    Port = os:getenv("KUBERNETES_SERVICE_PORT"),
    {ok, Token} = file:read_file("/var/run/secrets/kubernetes.io/serviceaccount/token"),
    #server{
        url = "https://" ++ Host ++ ":" ++ Port,
        ca_cert = cert_from_file("/var/run/secrets/kubernetes.io/serviceaccount/ca.crt"),
        auth = #auth_token{token = Token}
    }.

%% @doc
%% Initiates a GET request against the configured kubernetes api server
%% or raises an error if the server is not configured correctly.
%% @end
-spec get(Path, Opts) -> Body when
    Path :: string(),
    Opts :: options(),
    Body :: map().
get(Path, Opts) ->
    Server = get_server(Opts),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(
        get,
        {Server#server.url ++ Path, headers(Server)},
        [{ssl, [{cacerts, [Server#server.ca_cert]}]}],
        []
    ),
    decode(Body).

%% @doc
%% Initiates a POST request against the configured kubernetes api server or
%% raises an error if the server is not configured correctly.
%% see: server()
%% @end
-spec create(Request, Opts) -> ok when
    Request :: mutation_request(),
    Opts :: options().
create(#{path := Path, body := Body}, Opts) ->
    Server = get_server(Opts),
    {ok, {{_, 201, _}, _, _}} = httpc:request(
        post,
        {Server#server.url ++ Path, headers(Server), "application/json", jsone:encode(Body)},
        [{ssl, [{cacerts, [Server#server.ca_cert]}]}],
        []
    ),
    ok.

%% @doc
%%   method: "patch",
%%  path: "/api/v1/nodes/some-node",
%%  query_params: %{},
%%  content_type: "application/merge-patch+json",
%%  body: "{\"spec\":{\"taints\":[{\"value\":\"123\",\"key\":\"aksnth/Preempt\",\"effect\":\"NoSchedule\"}]}}",
%%  response_schema: Kazan.Apis.Core.V1.Node
%% @end
-spec patch(Request, Opts) -> ok when
    Request :: mutation_request(),
    Opts :: options().
patch(#{path := Path, body := Body}, Opts) ->
    Server = get_server(Opts),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        patch,
        {
            Server#server.url ++ Path,
            headers(Server),
            "application/merge-patch+json",
            jsone:encode(Body)
        },
        [{ssl, [{cacerts, [Server#server.ca_cert]}]}],
        []
    ),
    ok.

%% @doc
%% Returns the current datetime in the kubernetes MicroTime format
%% see: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.22/#microtime-v1-meta
%% @end
-spec microtime_now() -> binary().
microtime_now() ->
    erlang:list_to_binary(
        calendar:system_time_to_rfc3339(erlang:system_time(microsecond), [
            {unit, microsecond}, {offset, "Z"}
        ])
    ).

%% internal functions
headers(Server) -> headers(Server, []).
headers(#server{auth = #auth_token{token = Token}}, AdditionalHeaders) ->
    [
        {"Accept", "application/json"},
        {"Authorization", "Bearer " ++ Token}
    ] ++ AdditionalHeaders.

cert_from_file(Path) ->
    true = filelib:is_file(Path),
    {ok, BinaryContent} = file:read_file(Path),
    [{'Certificate', Data, _}] = public_key:pem_decode(BinaryContent),
    Data.

cert_from_b64(CertData) when is_list(CertData) ->
    DecodedCert = base64:decode(CertData),
    [{'Certificate', Data, _}] = public_key:pem_decode(DecodedCert),
    Data.

get_server(Opts) ->
    OptsServer = maps:get(server, Opts, undefined),
    case OptsServer of
        undefined -> error(no_server_configured);
        Else when is_record(Else, server) -> Else;
        _ -> error(invalid_server)
    end.

decode(Body) when is_binary(Body) -> jsone:decode(Body);
decode(Body) when is_list(Body) -> jsone:decode(erlang:list_to_binary(Body)).
