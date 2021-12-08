-module(gaia_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]). %% Used by supervisor:start_link/2

-include_lib("apptools/include/shorthand.hrl").

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
    GaiaId = config:lookup([gaia, 'gaia-id']),
    BindAddress = config:lookup([gaia, 'bind-address']),
    CapturePcmName = ?b2l(config:lookup([gaia, 'capture-pcm-name'])),
    PlaybackPcmName = ?b2l(config:lookup([gaia, 'playback-pcm-name'])),
    GaiaServ =
	#{id => gaia_serv,
          start => {gaia_serv, start_link,
                    [GaiaId, BindAddress, PlaybackPcmName]}},
    GaiaAudioSourceServ =
	#{id => gaia_audio_source_serv,
          start => {gaia_audio_source_serv, start_link,
		    [[{device, CapturePcmName}]]}},
    %% WARNING: To start the audio sink server playback_audio *must* be
    %% set to false in ../c_src/audio_sink.c
    _GaiaAudioSinkServ =
	#{id => gaia_audio_sink_serv,
          start => {gaia_audio_sink_serv, start_link,
		    [[{device, PlaybackPcmName}]]}},
    GaiaNetworkSenderServ =
	#{id => gaia_network_sender_serv,
          start => {gaia_network_sender_serv, start_link,
                    [GaiaId, BindAddress, true]}},
    {ok, {#{strategy => one_for_all},
          [GaiaServ, GaiaAudioSourceServ, GaiaNetworkSenderServ]}}.
