-module(gaia_serv).
-export([start_link/3, stop/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("gaia.hrl").

%%
%% Exported: start_link
%%

start_link(GaiaId, BindAddress, PcmName) ->
    ?spawn_server(fun(Parent) -> init(Parent, GaiaId, BindAddress, PcmName) end,
                  fun initial_message_handler/1).

%%
%% Exported: stop
%%

stop(Pid) ->
    serv:call(Pid, stop).

%%
%% Server
%%

init(Parent, GaiaId, {IpAddress, Port}, PcmName) ->
    ?LOG_INFO("Gaia NIF is initializing..."),
    ok = gaia_nif:start({#{addr_port => {inet_parse:ntoa(IpAddress), Port},
                           opus_enabled => false},
                         #{pcm_name => PcmName,
                           opus_enabled => false}}),
    ?LOG_INFO("Gaia NIF has been initialized"),
    ?LOG_DEBUG(#{module => ?MODULE, gaia_address => IpAddress}),
    ok = nodis:set_node_info(
           #{gaia => #{id => GaiaId,
                       ip_address => inet:ntoa(IpAddress),
                       port => Port}}),
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
            %ok = gaia_network_sender_serv:set_addresses(
            %       NetworkSenderPid,
            %       [{?DEFAULT_ADDR, ?DEFAULT_PORT}]),
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
                  fun(_Address,
                      #{ip_address := GaiaIpAddress, port := GaiaPort}, Acc) ->
                          [{GaiaIpAddress, GaiaPort}|Acc]
                  end, [], UpdatedNeighbours),
            ok = gaia_network_sender_serv:set_dest_addresses(
                   NetworkSenderPid, DestAddresses),
            {noreply, State#{neighbours => UpdatedNeighbours}};
        {nodis, NodisSubscription, {wait, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {wait, Address}}),
            noreply;
        {nodis, NodisSubscription, {change, Address, Info}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {change, Address, Info}}),
            case update_neighbours(Neighbours, Address, Info) of
                not_updated ->
                    noreply;
                UpdatedNeighbours ->
                    DestAddresses =
                        maps:fold(
                          fun(_Address,
                              #{ip_address := GaiaIpAddress, port := GaiaPort},
                              Acc) ->
                                  [{GaiaIpAddress, GaiaPort}|Acc]
                          end, [], UpdatedNeighbours),
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
    case lists:keysearch(gaia, 1, Info) of
        {value, {gaia, #{id := GaiaId,
                         ip_address := GaiaIpAddressString,
                         port := GaiaPort} = GaiaInfo, _PreviousGaiaInfo}}
          when is_integer(GaiaId) andalso
               GaiaId > 0 andalso GaiaId < 65536 andalso
               is_integer(GaiaPort) andalso
               GaiaPort >= 1024 andalso GaiaPort < 65536 ->
            case inet:parse_address(GaiaIpAddressString) of
                {ok, GaiaIpAddress} ->
                    Neighbours#{Address =>
                                    GaiaInfo#{ip_address =>
                                                  GaiaIpAddress}};
                {error, _Reason} ->
                    ?LOG_WARNING(#{module => ?MODULE,
                                   bad_gaia_info => GaiaInfo}),
                    not_updated
            end;
        {value, {gaia, GaiaInfo, _PreviousGaiaInfo}} when is_map(GaiaInfo) ->
            ?LOG_WARNING(#{module => ?MODULE, bad_gaia_info => GaiaInfo}),
            not_updated;
        false ->
            not_updated
    end.
