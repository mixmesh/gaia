-module(gaia_serv).
-export([start_link/3, stop/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("globals.hrl").

%%
%% Exported: start_link
%%

start_link(GaiaId, Port, PcmName) ->
    ?spawn_server(fun(Parent) -> init(Parent, GaiaId, Port, PcmName) end,
                  fun initial_message_handler/1).

%%
%% Exported: stop
%%

stop(Pid) ->
    serv:call(Pid, stop).

%%
%% Server
%%

init(Parent, -1, Port, PcmName) ->
    [{nodeid, Nodeid}] = nodis:get_node_info([nodeid]),
    GaiaId =
        binary:decode_unsigned(binary:part(Nodeid, {byte_size(Nodeid), -4})),
    init(Parent, GaiaId, Port, PcmName);
init(Parent, GaiaId, Port, PcmName) ->
    ?LOG_INFO("Gaia NIF is initializing..."),
    ok = gaia_nif:start({#{port => Port},
                         #{pcm_name => PcmName,
                           playback_audio => ?PLAYBACK_AUDIO}}),
    ?LOG_INFO("Gaia NIF has been initialized"),
    ok = nodis:set_node_info(#{gaia => #{id => GaiaId, port => Port}}),
    ?LOG_INFO("Gaia server has been started"),
    {ok, NodisSubscription} = nodis_serv:subscribe(),
    {ok, #{parent => Parent,
           gaia_id => GaiaId,
           nodis_subscription => NodisSubscription,
           neighbours => #{}}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, NeighbourWorkers} ->
            [NetworkSenderPid] =
                supervisor_helper:get_selected_worker_pids(
                  [gaia_network_sender_serv], NeighbourWorkers),
            %% DEBUG
            %%ok = gaia_network_sender_serv:set_dest_addresses(
            %%       NetworkSenderPid, [{?DEFAULT_ADDR, ?DEFAULT_PORT}]),
            {swap_message_handler, fun ?MODULE:message_handler/1,
             State#{network_sender_pid => NetworkSenderPid}}
    end.

message_handler(#{parent := Parent,
                  network_sender_pid := NetworkSenderPid,
                  nodis_subscription := NodisSubscription,
                  neighbours := Neighbours} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {nodis, NodisSubscription, {pending, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {pending, Address}}),
            noreply;
        {nodis, NodisSubscription, {up, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {up, Address}}),
            noreply;
        {nodis, NodisSubscription, {down, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {down, Address}}),
            UpdatedNeighbours = maps:remove(Address, Neighbours),
            DestAddresses =
                maps:fold(
                  fun({IpAddress, _SyncPort}, #{port := GaiaPort}, Acc) ->
                          [{IpAddress, GaiaPort}|Acc]
                  end, [], UpdatedNeighbours),
            ok = gaia_network_sender_serv:set_dest_addresses(
                   NetworkSenderPid, DestAddresses),
            {noreply, State#{neighbours => UpdatedNeighbours}};
        {nodis, NodisSubscription, {wait, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {wait, Address}}),
            noreply;
        {nodis, NodisSubscription, {change, Address, Info}} ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         nodis => change,
                         node_info => {Address, Info}}),
            case update_neighbours(Neighbours, Address, Info) of
                not_updated ->
                    ?LOG_DEBUG(
                       #{module => ?MODULE,
                         nodis => {not_updated, Neighbours, Address, Info}}),
                    noreply;
                UpdatedNeighbours ->
                    DestAddresses =
                        maps:fold(
                          fun({IpAddress, _SyncPort}, #{port := GaiaPort},
                              Acc) ->
                                  [{IpAddress, GaiaPort}|Acc]
                          end, [], UpdatedNeighbours),
                    ?LOG_DEBUG(#{module => ?MODULE,
                                 sets_dest_addresses => {UpdatedNeighbours,
                                                         DestAddresses}}),
                    ok = gaia_network_sender_serv:set_dest_addresses(
                           NetworkSenderPid, DestAddresses),
                    {noreply, State#{neighbours => UpdatedNeighbours}}
            end;
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

update_neighbours(Neighbours, Address, Info) ->
    MaxGaiaId = math:pow(2,32) - 1,
    case lists:keysearch(gaia, 1, Info) of
        {value, {gaia, #{id := GaiaId, port := GaiaPort} = GaiaInfo,
                 _PreviousGaiaInfo}}
          when is_integer(GaiaId) andalso
               GaiaId > 0 andalso GaiaId =< MaxGaiaId andalso
               is_integer(GaiaPort) andalso
               GaiaPort >= 1024 andalso GaiaPort < 65536 ->
            Neighbours#{Address => GaiaInfo};
        {value, {gaia, GaiaInfo, _PreviousGaiaInfo}} when is_map(GaiaInfo) ->
            ?LOG_WARNING(#{module => ?MODULE, bad_gaia_info => GaiaInfo}),
            not_updated;
        false ->
            ?LOG_ERROR(#{module => ?MODULE, invalid_gaia_info => Info}),
            not_updated
    end.
