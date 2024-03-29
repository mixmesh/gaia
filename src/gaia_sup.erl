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
              fun(gaia_pa_serv, _Pid, _NeighbourWorkers) ->
                      ok;
                 (_Id, Pid, NeighbourWorkers) ->
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
    [PeerId, PeerName, RestPort, UseOpusCodec, CapturePcmName,
     PlaybackPcmName] =
        config:lookup_children(
          ['peer-id', 'peer-name', 'rest-port', 'use-opus-codec',
           'capture-pcm-name', 'playback-pcm-name'], ConfigGaia),
    GaiaPaServ =
	#{id => gaia_pa_serv,
          start => {gaia_pa_serv, start_link, []}},
    ListenAlways = false,
    GaiaServ =
	#{id => gaia_serv,
          start => {gaia_serv, start_link,
                    [GaiaDir, PeerId, PeerName, RestPort,
                     ?b2l(PlaybackPcmName), ListenAlways]}},
    GaiaRestService =
	#{id => gaia_rest_service,
          start => {gaia_rest_service, start_link, [PeerId, RestPort]}},
    GaiaAudioSourceServ =
	#{id => gaia_audio_source_serv,
          start => {gaia_audio_source_serv, start_link,
		    [[{device, ?b2l(CapturePcmName)}]]}},
    %% WARNING: To start the audio sink server PLAYBACK_AUDIO *must* be
    %% set to false in globals.hrl
    _GaiaAudioSinkServ =
	#{id => gaia_audio_sink_serv,
          start => {gaia_audio_sink_serv, start_link,
		    [[{device, ?b2l(PlaybackPcmName)}]]}},
    GaiaNetworkSenderServ =
	#{id => gaia_network_sender_serv,
          start => {gaia_network_sender_serv, start_link,
                    [PeerId, true, UseOpusCodec]}},
    GaiaAsrServ =
	#{id => gaia_asr_serv,
	  start => {gaia_asr_serv, start_link, [ListenAlways]}},
    GaiaTtsServ =
	#{id => gaia_tts_serv,
          start => {gaia_tts_serv, start_link, []}},
    {ok, {#{strategy => one_for_all},
          [GaiaPaServ,
           GaiaServ,
           GaiaRestService,
           GaiaAudioSourceServ,
           GaiaNetworkSenderServ,
           GaiaAsrServ,
           GaiaTtsServ]}}.
