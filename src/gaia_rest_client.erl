-module(gaia_rest_client).
-export([start_peer_negotiation/3, get_group/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/gaia_serv.hrl").

-define(HTTPC_TIMEOUT, 2000).

%%
%% Exported: start_peer_negotiation
%%

-spec start_peer_negotiation(gaia_serv:peer_id(),
                             {inet:ip_address(), inet:port_number()},
                             inet:port_number()) ->
          {ok, inet:port_number()} | asking | busy | not_available |
          {error,
           {invalid_response_body, jsone:json_value()} |
           {invalid_json_syntax, ResponseBody :: binary()} |
           {bad_request, Reason :: binary()} |
           {invalid_response, Response :: term()} |
           {http_error, Reason :: term()}}.

start_peer_negotiation(MyPeerId, Address, LocalPort) ->
    RequestBody = encode_json(#{<<"port">> => LocalPort}),
    case request(MyPeerId, Address, "peer-negotiation", RequestBody, post) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseBody}} ->
            try jsone:decode(ResponseBody) of
                #{<<"port">> := Port} when is_integer(Port) ->
                    {ok, Port};
                JsonValue ->
                    {error, {invalid_response_body, JsonValue}}
            catch
                _:_ ->
                    {error, {invalid_json_syntax, ResponseBody}}
            end;
        {ok, {{_Version, 403, _ReasonPhrase}, _Headers, <<"Asking">>}} ->
            asking;
        {ok, {{_Version, 403, _ReasonPhrase}, _Headers, <<"Busy">>}} ->
            busy;
        {ok, {{_Version, 403, _ReasonPhrase}, _Headers, <<"Not Available">>}} ->
            not_available;
        {ok, {{_Version, 400, _ReasonPhrase}, _Headers, Reason}} ->
            {error, {bad_request, Reason}};
        {ok, Response} ->
            {error, {invalid_response, Response}};
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

%%
%% Exported: get_group
%%

-spec get_group(gaia_serv:peer_id(),
                gaia_serv:peer_id(),
                {inet:ip_address(), inet:port_number()},
                gaia_serv:group_id()) ->
          {ok, #gaia_group{}} |
          {error,
           {invalid_response_body, jsone:json_value()} |
           {invalid_json_syntax, ResponseBody :: binary()} |
           {invalid_response, Response :: term()} |
           {http_error, Reason :: term()}}.

get_group(MyPeerId, _Admin, Address, GroupId) ->
    RequestPath = "group/" ++ ?i2l(GroupId),
    case request(MyPeerId, Address, RequestPath, no_request_body, get) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseBody}} ->
            try jsone:decode(ResponseBody, [undefined_as_null]) of
                JsonValue ->
                    try create_group(JsonValue) of
                        Group ->
                            {ok, Group}
                    catch
                        _:_ ->
                            {error, {invalid_response_body, JsonValue}}
                    end
            catch
                _:_ ->
                    {error, {invalid_json_syntax, ResponseBody}}
            end;
        {ok, Response} ->
            {error, {invalid_response, Response}};
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

create_group(#{<<"id">> := Id,
               <<"name">> := Name,
               <<"public">> := Public,
               <<"multicast_ip_address">> := MulticastIpAddress,
               <<"port">> := Port,
               <<"type">> := Type,
               <<"members">> := Members,
               <<"admin">> := Admin,
               <<"session_key">> := SessionKey})
  when is_integer(Id) andalso
       is_binary(Name) andalso
       is_boolean(Public) andalso
       (MulticastIpAddress == undefined orelse
        is_binary(MulticastIpAddress)) andalso
       is_integer(Port) andalso
       (Type == <<"open">> orelse Type == <<"closed">>) andalso
       is_integer(Admin) andalso
       (SessionKey == undefined orelse is_binary(SessionKey)) ->
    #gaia_group{
       id = Id,
       name = Name,
       public = Public,
       multicast_ip_address = decode_multicast_ip_address(MulticastIpAddress),
       port = Port,
       type = ?b2a(Type),
       members = decode_members(Members),
       admin = Admin,
       session_key = SessionKey};
create_group(_ResponseBody) ->
    throw(invalid_group).

decode_multicast_ip_address(undefined) ->
    undefined;
decode_multicast_ip_address(IpAddressString) ->
    {ok, IpAddress} = inet:parse_address(?b2l(IpAddressString)),
    IpAddress.

decode_members(<<"*">>) ->
    '*';
decode_members(Members) ->
    true = lists:all(fun(Member) -> is_integer(Member) end, Members),
    Members.

%%
%% Request tools
%%

encode_json(JsonTerm) ->
    jsone:encode(JsonTerm, [{float_format, [{decimals, 4}, compact]},
                            {indent, 2},
                            {object_key_type, value},
                            {space, 1},
                            native_forward_slash,
                            undefined_as_null]).

request(MyPeerId, {IpAddress, RestPort}, RequestPath, RequestBody, Method) ->
    Url = lists:flatten(
            io_lib:format("http://~s:~w/~s",
                          [inet:ntoa(IpAddress), RestPort, RequestPath])),
    Nonce = ?b2l(keydir_service:bin_to_hexstr(<<"FIXME">>)),
    HMAC = ?b2l(keydir_service:bin_to_hexstr(<<"FIXME">>)),
    Request =
        case RequestBody of
            no_request_body ->
                {Url, [{"connection", "close"},
                       {"gaia-peer-id", ?i2l(MyPeerId)},
                       {"gaia-nonce", Nonce},
                       {"gaia-hmac", HMAC}]};
            _ ->
                {Url, [{"connection", "close"},
                       {"gaia-peer-id", ?i2l(MyPeerId)},
                       {"gaia-nonce", Nonce},
                       {"gaia-hmac", HMAC}],
                 "application/json",
                 RequestBody}
        end,
    httpc:request(Method, Request, [{timeout, ?HTTPC_TIMEOUT}],
                  [{body_format, binary}]).
