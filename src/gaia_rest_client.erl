-module(gaia_rest_client).
-export([start_peer_negotiation/3, get_group/3]).

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
          {ok, inet:port_number()} |
          {error,
           {invalid_response_body, jsone:json_value()} |
           {json_decode, Reason :: term()} |
           {bad_response, Result :: term()} |
           {http_error, Reason :: term()}}.

start_peer_negotiation(MyPeerId, Address, LocalPort) ->
    RequestBody = encode_json(#{<<"port">> => LocalPort}),
    case request(MyPeerId, Address, "peer-negotiation", RequestBody, post) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseBody}} ->
            try jsone:decode(ResponseBody) of
                {ok, #{<<"port">> := Port}, _} when is_integer(Port) ->
                    {ok, Port};
                {ok, JsonValue, _} ->
                    {error, {invalid_response_body, JsonValue}}
            catch
                _:_ ->
                    {error, {json_decode, invalid_json}}
            end;
        {ok, Result} ->
            {error, {bad_response, Result}};
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

%%
%% Exported: get_group
%%

-spec get_group(gaia_serv:peer_id(),
                {inet:ip_address(), inet:port_number()},
                gaia_serv:group_id()) ->
          {ok, #gaia_group{}} |
          {error,
           {invalid_response_body, jsone:json_value()} |
           {json_decode, Reason :: term()} |
           {bad_response, Result :: term()} |
           {http_error, Reason :: term()}}.

get_group(MyPeerId, Address, GroupId) ->
    RequestPath = "group/" ++ ?i2l(GroupId),
    case request(MyPeerId, Address, RequestPath, no_request_body, get) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseBody}} ->
            try jsone:decode(ResponseBody, [undefined_as_null]) of
                JsonValue ->
                    try create_group(JsonValue) of
                        Group ->
                            ?LOG_INFO(#{module => ?MODULE, group => Group}),
                            {ok, Group}
                    catch
                        Class:Error ->
                            ?LOG_ERROR(#{module => ?MODULE,
                                         catched => {Class, Error}}),
                            {error, {json_decode, JsonValue}}
                    end
            catch
                _:_ ->
                    {error, {json_decode, invalid_json}}
            end;
        {ok, Result} ->
            {error, {bad_response, Result}};
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

create_group(#{id := Id,
               name := Name,
               public := Public,
               multicast_ip_address := MulticastIpAddress,
               port := Port,
               type := Type,
               members := Members,
               admin := Admin,
               session_key := SessionKey})
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
create_group(ResponseBody) ->
    throw({invalid_group, ResponseBody}).

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
