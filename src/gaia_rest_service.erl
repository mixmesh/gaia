-module(gaia_rest_service).
-export([start_link/2]).
-export([handle_http_request/4]).
-export([negotiate_with_peer/3]).

-include_lib("kernel/include/logger.hrl").
-include_lib("rester/include/rester.hrl").
-include_lib("rester/include/rester_http.hrl").
-include("globals.hrl").

-define(HTTPC_TIMEOUT, 2000).

%%
%% Exported: start_link
%%

-spec start_link(gaia_serv:peer_id(), inet:port_number()) ->
          {ok, pid()} |  {error, Reason :: term()}.

start_link(PeerId, RestPort) ->
    %% FIXME: Use SSL
    ResterHttpArgs =
	[{request_handler,
	  {?MODULE, handle_http_request, [{peer_id, PeerId}]}},
	 {nodelay, true},
	 {reuseaddr, true}],
    ?LOG_INFO("Gaia REST service has been started"),
    rester_http_server:start_link(RestPort, ResterHttpArgs).

%%
%% Exported: negotiate_with_peer
%%

-spec negotiate_with_peer(gaia_serv:peer_id(),
                          {inet:ip_address(), inet:port_number()},
                          inet:port_number()) ->
          {ok, inet:port_number()} |
          {error,
           {invalid_response_body, jsone:json_value()} |
           {json_decode, Reason :: term()} |
           {bad_response, Result :: term()} |
           {http_error, Reason :: term()}}.

negotiate_with_peer(PeerId, {IpAddress, RestPort}, LocalPort) ->
    Url = io_lib:format("http://~s:~w/negotiate",
                        [inet:ntoa(IpAddress), RestPort]),
    RequestBody = encode_json([{<<"port">>, LocalPort}]),
    Nonce = <<"FIXME">>,
    HMAC = <<"FIXME">>,
    case httpc:request(
           post, {Url,
                  [{"gaia-peer-id", binary:encode_unsigned(PeerId)},
                   {"gaia-nonce", Nonce},
                   {"gaia-hmac", HMAC}],
                  "application/json",
                  RequestBody},
           [{timeout, ?HTTPC_TIMEOUT}],
           [{body_format, binary}],
           ?MODULE) of
        {ok, {200, _ResponseHeaders, ResponseBody}} ->
            try
                case jsone:try_decode(ResponseBody) of
                    {ok, [{<<"port">>, Port}], _} when is_integer(Port) ->
                        {ok, Port};
                    {ok, JsonValue} ->
                        {error, {invalid_response_body, JsonValue}}
                end
            catch
                error:Reason ->
                    {error, {json_decode, Reason}}
            end;
        {ok, Result} ->
            {error, {bad_response, Result}};
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

encode_json(JsonTerm) ->
    jsone:encode(JsonTerm, [{float_format, [{decimals, 4}, compact]},
                            {indent, 2},
                            {object_key_type, value},
                            {space, 1},
                            native_forward_slash]).

%%
%% Exported: handle_http_request
%%

handle_http_request(Socket, Request, Body, Options) ->
    ?LOG_DEBUG(
       #{module => ?MODULE,
         request => rester_http:format_request(Request),
         headers => rester_http:format_hdr(Request#http_request.headers),
         body => Body,
         options => Options}),
    try
        case Request#http_request.method of
            'POST' ->
                handle_http_post(Socket, Request, Body, Options);
            _ ->
                rest_util:response(Socket, Request, {error, not_allowed})
        end
    catch
	_Class:Reason:StackTrace ->
	    ?LOG_ERROR(#{module => ?MODULE,
                         crash => Reason,
                         stack_trace => StackTrace}),
	    erlang:error(Reason)
    end.

handle_http_post(Socket, Request, Body, _Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
        %% POST http://192.167.7.8/negotiate\r\n
        %%   gaia-peer-id: ...\r\n
        %%   gaia-nonce: ...\r\n
        %%   gaia-hmac: ...\r\n\r\n
	["negotiate"] ->
            case rest_util:parse_body(
                   Request, Body,
                   [{jsone_options, [{object_format, proplist}]}]) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                [{<<"port">>, Port}] when is_integer(Port) ->
                    rest_util:response(
                      Socket, Request, negotiate_post(Request, Port));
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid request body"})
            end;
	Tokens ->
	    ?LOG_INFO(#{module => ?MODULE, unknown_post_path => Tokens}),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

negotiate_post(Request, RemotePort) ->
    case get_gaia_headers(Request) of
        {ok, PeerId, _Nonce, _HMAC} ->
            case gaia_serv:peer_wants_to_negotiate(PeerId, RemotePort) of
                {ok, LocalPort} ->
                    {ok, {format, [{<<"port">>, LocalPort}]}};
                {error, no_such_peer_id} ->
                    {error, {bad_request, "Unknown peer"}}
            end;
        missing_headers ->
            {error, {bad_request, "Missing GAIA HTTP headers"}}
    end.

get_gaia_headers(_Request) ->
    'FIXME'.
