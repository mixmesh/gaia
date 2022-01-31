-module(gaia_nif).
-export([start/1, stop/0, set_params/1, read_packet/0, update_conversations/1]).
-export([init/0, preloaded_atoms/0]). % internal
-on_load(init/0).

-include("globals.hrl").

-type network_receiver_params() ::
        #{addr_port := {inet:ip_address(), inet:port_number()},
          opus_enabled := boolean()}.
-type audio_sink_params() ::
        #{pcm_name := string(),
          opus_enabled := boolean(),
          playback_audio := boolean()}.
-type params() :: {network_receiver_params(), audio_sink_params()}.

%%
%% Exported: init
%%

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(gaia), gaia_nif), undefined).

%%
%% Exported: preloaded_atoms
%%

preloaded_atoms() ->
    [already_started, not_started, bad_params, port, playback_audio, pcm_name].

%%
%% Exported: start
%%

-spec start(params()) -> ok.


start(_Params) ->
    exit(nif_library_not_loaded).

%%
%% Exported: start
%%

-spec stop() -> ok.

stop() ->
    exit(nif_library_not_loaded).

%%
%% Exported: set_params
%%

-spec set_params(params()) -> ok.

set_params(_Params) ->
    exit(nif_library_not_loaded).

%%
%% Exported: read_packet
%%

-spec read_packet() -> binary().

read_packet() ->
    exit(nif_library_not_loaded).

%%
%% Exported: set_conversations
%%

-spec update_conversations(gaia_serv:conversations()) ->
          [{{peer, gaia_serv:peer_id()}, inet:port_number()}|
           {{group, gaia_serv:group_id()}, inet:port_number()}].

update_conversations(_Conversations) ->
    exit(nif_library_not_loaded).
