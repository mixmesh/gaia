-module(gaia_nif).
-export([start/0, start/1, stop/0, set_params/1]).
-export([init/0, preloaded_atoms/0]). % internal
-on_load(init/0).

-include("gaia.hrl").

-type network_receiver_params() ::
        #{addr_port := {inet:ip_address(), inet:port_number()},
          opus_enabled := boolean()}.
-type audio_sink_params() ::
        #{pcm_name := string(),
          opus_enabled := boolean()}.
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
    [already_started, not_started, bad_params, addr_port, opus_enabled,
     pcm_name].

%%
%% Exported: start
%%

-spec start(params()) -> ok.

start() ->
    start({#{addr_port => {?DEFAULT_ADDR, ?DEFAULT_PORT},
             opus_enabled => false},
           #{pcm_name => "hw:0,0",
             opus_enabled => false}}).

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
