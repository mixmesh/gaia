-module(gaia_rest_service).
-export([start_link/1]).
-export([handle_http_request/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("rester/include/rester.hrl").
-include_lib("rester/include/rester_http.hrl").
-include("globals.hrl").

%%
%% Exported: start_link
%%

start_link(GaiaId) ->
    ResterHttpArgs =
	[{request_handler,
	  {?MODULE, handle_http_request, [{gaia_id, GaiaId}]}},
	 {nodelay, true},
	 {reuseaddr, true}],
    ?LOG_INFO("Gaia REST service has been started"),
    rester_http_server:start_link(?DEFAULT_REST_PORT, ResterHttpArgs).

%%
%% Exported: handle_http_request
%%

handle_http_request(Socket, Request, Body, Options) ->
    ?LOG_DEBUG(#{module => ?MODULE,
                 request => rester_http:format_request(Request),
                 headers => rester_http:format_hdr(Request#http_request.headers),
                 body => Body}),
    try
        case Request#http_request.method of
            'GET' ->
                handle_http_get(Socket, Request, Body, Options);
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

handle_http_get(Socket, Request, _Body, _Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
        %% GET http://192.167.7.8:443/contact/johanna
        %%   my-gaia-id: ...\r\n
        %%   nonce: ...\r\n
        %%   hmac: ...\r\n\r\n
        %% contains a fresh contact sheet, i.e. with a
        %% session-key. The contact sheet with the "largest" gaia-id wins.
	["contact", _Name] ->
            rest_util:response(Socket, Request, {error, not_found});
        %% GET http://192.167.7.14:443/group/barnen
        %%   my-gaia-id: ...\r\n
        %%   nonce: ...\r\n
        %%   hmac: ...\r\n\r\n
        %% contains a fresh group sheet, i.e. with a session-key and a
        %% list of members.
        ["group", Name] ->
            {ok, JsonTerm} = config_serv:get_env([gaia, groups, {name, Name}]),
            {ok, {format, JsonTerm}};
	Tokens ->
	    ?LOG_INFO(#{module => ?MODULE, unknown_get_path => Tokens}),
	    rest_util:response(Socket, Request, {error, not_found})
    end.
