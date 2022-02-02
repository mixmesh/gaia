-module(gaia_rest_service).
-export([start_link/2]).
-export([handle_http_request/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("rester/include/rester_http.hrl").
-include_lib("apptools/include/shorthand.hrl").

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
    ?LOG_INFO("Gaia REST service has been started on port 0.0.0.0:~w",
              [RestPort]),
    rester_http_server:start_link(RestPort, ResterHttpArgs).

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
        %% POST http://192.167.7.8:8787/peer-negotiation\r\n
        %%   gaia-peer-id: ...\r\n
        %%   gaia-nonce: ...\r\n
        %%   gaia-hmac: ...\r\n\r\n
	["peer-negotiation"] ->
            case rest_util:parse_body(Request, Body,
                                      [{jsone_options, [undefined_as_null]}]) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                #{<<"port">> := Port} when is_integer(Port) ->
                    rest_util:response(
                      Socket, Request, peer_negotiation(Request, Port));
                _ ->
                    ?LOG_ERROR(#{module => ?MODULE,
                                 invalid_request_body => Body}),
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid request body"})
            end;
	Tokens ->
	    ?LOG_INFO(#{module => ?MODULE, unknown_post_path => Tokens}),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

peer_negotiation(#http_request{headers = #http_chdr{other = Headers}},
                 RemotePort) ->
    case get_gaia_headers(Headers) of
        {PeerId, _Nonce, _HMAC} when PeerId /= not_set ->
            case gaia_serv:handle_peer_negotiation(PeerId, RemotePort) of
                {ok, LocalPort} ->
                    {ok, {format, [{<<"port">>, LocalPort}]}};
                {error, Reason} ->
                    ?LOG_INFO(#{module => ?MODULE,
                                peer_negotiation_rejected => Reason}),
                    NoAccessBody =
                        case Reason of
                            ask ->
                                "Asking";
                            busy ->
                                "Busy";
                            _ ->
                                "Not Available"
                        end,
                    {error, {no_access, NoAccessBody}}
            end;
        _ ->
            {error, {bad_request, "Missing GAIA HTTP headers"}}
    end.

get_gaia_headers(Headers) ->
    get_gaia_headers(Headers, {not_found, not_found, not_found}).

get_gaia_headers([], Acc) ->
    Acc;
get_gaia_headers([{"Gaia-Peer-Id", PeerId}|Rest], {_, Nonce, HMAC}) ->
    try
        get_gaia_headers(Rest, {?l2i(PeerId), Nonce, HMAC})
    catch
        error:badarg ->
            invalid_peer_id
    end;
get_gaia_headers([_|Rest], Acc) ->
    get_gaia_headers(Rest, Acc).
