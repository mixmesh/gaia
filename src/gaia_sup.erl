-module(gaia_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]). %% Used by supervisor:start_link/2

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
    InterfaceIpAddress = config:lookup([gaia, 'interface-ip-address']),
    BindAddress = config:lookup([gaia, 'bind-address']),
    GaiaServ =
	#{id => gaia_serv,
          start => {gaia_serv, start_link, [BindAddress]}},
    GaiaAudioSourceServ =
	#{id => gaia_audio_source_serv,
          start => {gaia_audio_source_serv, start_link, []}},
    GaiaNetworkSenderServ =
	#{id => gaia_network_sender_serv,
          start => {gaia_network_sender_serv, start_link,
		    [GaiaId, InterfaceIpAddress, false]}},
    {ok, {#{strategy => one_for_all},
          [GaiaServ, GaiaAudioSourceServ, GaiaNetworkSenderServ]}}.
