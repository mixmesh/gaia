-module(gaia_rest_service).
-export([start_link/2]).
-export([handle_http_request/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("rester/include/rester_http.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/gaia_serv.hrl").

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
       #{request => rester_http:format_request(Request),
         headers => rester_http:format_hdr(Request#http_request.headers),
         body => Body,
         options => Options}),
    try Request#http_request.method of
        'GET' ->
            handle_http_get(Socket, Request, Body, Options);
        'POST' ->
            handle_http_post(Socket, Request, Body, Options);
        _ ->
            rest_util:response(Socket, Request, {error, not_allowed})
    catch
	_Class:Reason:StackTrace ->
	    ?LOG_ERROR(#{crash => Reason,
                         stack_trace => StackTrace}),
	    erlang:error(Reason)
    end.

%%
%% GET
%%

handle_http_get(Socket, Request, _Body, _Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["group", GroupIdString] ->
            Response =
                try ?l2i(GroupIdString) of
                    GroupId ->
                        Group = get_group(Request, GroupId),
                        ?LOG_INFO(#{group => Group}),
                        Group
                catch
                    _:_ ->
                        {error, bad_request, "Invalid Group ID"}
                end,
            rest_util:response(Socket, Request, Response);
        Tokens ->
	    ?LOG_ERROR(#{unknown_get_path => Tokens}),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

get_group(
  #http_request{headers = #http_chdr{other = Headers}}, GroupId) ->
    case get_gaia_headers(Headers) of
        {PeerId, _Nonce, _HMAC} when PeerId /= not_set ->
            case gaia_serv:lookup(GroupId) of
                [#gaia_group{
                    id = Id,
                    name = Name,
                    public = Public,
                    multicast_ip_address = MulticastIpAddress,
                    port = Port,
                    type = Type,
                    members = Members,
                    admin = Admin,
                    session_key = SessionKey}] ->
                    case encode_members(PeerId, Members) of
                        {ok, EncodedMembers} ->
                            ResponseBody =
                                #{<<"id">> => Id,
                                  <<"name">> => Name,
                                  <<"public">> => Public,
                                  <<"multicast_ip_address">> =>
                                      encode_multicast_ip_address(
                                        MulticastIpAddress),
                                  <<"port">> => Port,
                                  <<"type">> => ?a2b(Type),
                                  <<"members">> => EncodedMembers,
                                  <<"admin">> => Admin,
                                  <<"session_key">> => SessionKey},
                            {ok, {format, ResponseBody}};
                        {error, not_member} ->
                            {error, not_found}
                    end;
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, {bad_request, "Bad GAIA HTTP headers"}}
    end.

encode_multicast_ip_address(undefined) ->
    undefined;
encode_multicast_ip_address(IpAddress) ->
    inet:ntoa(IpAddress).

encode_members(_PeerId, '*') ->
    {ok, <<"*">>};
encode_members(PeerId, Members) ->
    case lists:member(PeerId, Members) of
        true ->
            {ok, Members};
        false ->
            {error, not_member}
    end.

%%
%% POST
%%

handle_http_post(Socket, Request, Body, _Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["start-of-conversation"] ->
            Response =
                case rest_util:parse_body(
                       Request, Body,
                       [{jsone_options, [undefined_as_null]}]) of
                    {error, _Reason} ->
                        {error, bad_request, "Invalid JSON format"};
                    #{<<"port">> := Port} when is_integer(Port) ->
                        start_of_conversation_post(Request, Port);
                    _ ->
                        ?LOG_ERROR(#{invalid_request_body => Body}),
                        {error, bad_request, "Invalid request body"}
                end,
            rest_util:response(Socket, Request, Response);
	["stop-of-conversation"] ->
            Response = stop_of_conversation_post(Request),
            rest_util:response(Socket, Request, Response);
	Tokens ->
	    ?LOG_ERROR(#{unknown_post_path => Tokens}),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

start_of_conversation_post(
  #http_request{headers = #http_chdr{other = Headers}}, RemotePort) ->
    case get_gaia_headers(Headers) of
        {PeerId, _Nonce, _HMAC} when PeerId /= not_set ->
            case gaia_serv:handle_start_of_conversation(PeerId, RemotePort) of
                {ok, LocalPort} ->
                    {ok, {format, #{<<"port">> => LocalPort}}};
                {error, Reason} ->
                    ?LOG_INFO(#{start_of_conversation_rejected => Reason}),
                    NoAccessBody =
                        case Reason of
                            call ->
                                "Calling";
                            busy ->
                                "Busy";
                            not_available ->
                                "Not Available"
                        end,
                    {error, {no_access, NoAccessBody}}
            end;
        _ ->
            {error, {bad_request, "Bad GAIA HTTP headers"}}
    end.

get_gaia_headers(Headers) ->
    get_gaia_headers(Headers, {not_found, not_found, not_found}).

get_gaia_headers([], Acc) ->
    Acc;
get_gaia_headers([{"Gaia-Nonce", _Nonce}|Rest], Acc) ->
    get_gaia_headers(Rest, Acc);
get_gaia_headers([{"Gaia-Nonce-Hmac", _HMAC}|Rest], Acc) ->
    get_gaia_headers(Rest, Acc);
get_gaia_headers([{"Gaia-Peer-Id", PeerId}|Rest], {_, Nonce, HMAC}) ->
    try ?l2i(PeerId) of
        DecodedPeerId ->
            get_gaia_headers(Rest, {DecodedPeerId, Nonce, HMAC})
    catch
        error:badarg ->
            invalid_peer_id
    end;
get_gaia_headers([_|Rest], Acc) ->
    get_gaia_headers(Rest, Acc).

stop_of_conversation_post(
  #http_request{headers = #http_chdr{other = Headers}}) ->
    case get_gaia_headers(Headers) of
        {PeerId, _Nonce, _HMAC} when PeerId /= not_set ->
            case gaia_serv:handle_stop_of_conversation(PeerId) of
                ok ->
                    ok_204;
                {error, Reason} ->
                    ?LOG_INFO(#{stop_of_conversation_rejected => Reason}),
                    ok_204
            end;
        _ ->
            {error, {bad_request, "Bad GAIA HTTP headers"}}
    end.
