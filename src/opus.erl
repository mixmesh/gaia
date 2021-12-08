-module(opus).
-export([create_encoder/4, encode/3]).
-export([init/0, preloaded_atoms/0]). % internal
-on_load(init/0).

-type encoder() :: term().
-type application() :: voip | audio | restricted_lowdelay.

%%
%% Exported: init
%%

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(gaia), opus_nif), undefined).

%%
%% Exported: preloaded_atoms
%%

preloaded_atoms() ->
    [voip, audio, restricted_lowdelay].

%%
%% Exported: create_encoder
%%

-spec create_encoder(integer(), integer(), application(), integer()) ->
          {ok, encoder()}.

create_encoder(_RateInHz, _Channels, _Application, _Complexity) ->
    exit(nif_library_not_loaded).

%%
%% Exported: encode
%%

-spec encode(encoder(), integer(), binary()) ->
          {ok, binary()}.

encode(_OpusEncoder, _MaxPacketSize, _Packet) ->
    exit(nif_library_not_loaded).
