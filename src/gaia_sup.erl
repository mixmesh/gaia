-module(gaia_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]). %% Used by supervisor:start_link/2

-include_lib("apptools/include/shorthand.hrl").
-include("globals.hrl").

%%
%% Exported: start_link
%%

-spec start_link() -> any().

start_link() ->
    case supervisor:start_link(?MODULE, []) of
        {ok, SupervisorPid} ->
            supervisor_helper:foreach_worker(
              SupervisorPid,
              fun(_Id, Pid, NeighbourWorkers) ->
                      Pid ! {neighbour_workers, NeighbourWorkers}
              end),
            {ok, SupervisorPid};
        Error ->
            Error
    end.

%%
%% Exported: init
%%

init([]) ->
    MixmeshDir = config:lookup([system, 'mixmesh-dir']),
    GaiaDir = filename:join([MixmeshDir, <<"gaia">>]),
    ConfigGaia = config:lookup([gaia]),
    [PeerName, PeerId, RestPort, UseOpusCodec, CapturePcmName,
     PlaybackPcmName] =
        config:lookup_children(
          ['peer-name', 'peer-id', 'rest-port', 'use-opus-codec',
           'capture-pcm-name', 'playback-pcm-name'], ConfigGaia),
    GaiaServ =
	#{id => gaia_serv,
          start => {gaia_serv, start_link,
                    [GaiaDir, PeerName, PeerId, RestPort,
                     ?b2l(PlaybackPcmName)]}},
    GaiaRestService =
	#{id => gaia_rest_service,
          start => {gaia_rest_service, start_link, [PeerId, RestPort]}},
    GaiaAudioSourceServ =
	#{id => gaia_audio_source_serv,
          start => {gaia_audio_source_serv, start_link,
		    [[{device, ?b2l(CapturePcmName)}]]}},
    %% WARNING: To start the audio sink server playback_audio *must* be
    %% set to false in ../c_src/audio_sink.c
    _GaiaAudioSinkServ =
	#{id => gaia_audio_sink_serv,
          start => {gaia_audio_sink_serv, start_link,
		    [[{device, ?b2l(PlaybackPcmName)}]]}},
    GaiaNetworkSenderServ =
	#{id => gaia_network_sender_serv,
          start => {gaia_network_sender_serv, start_link,
                    [PeerId, _UseCallback = true, UseOpusCodec]}},
    {ok, {#{strategy => one_for_all},
          [GaiaServ,
           GaiaRestService,
           GaiaAudioSourceServ,
           GaiaNetworkSenderServ]}}.
