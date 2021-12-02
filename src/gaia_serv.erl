-module(gaia_serv).
-export([start_link/1, stop/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("gaia.hrl").

%%
%% Exported: start_link
%%

start_link(BindIpPort) ->
    ?spawn_server(fun(Parent) -> init(Parent, BindIpPort) end,
                  fun initial_message_handler/1).

%%
%% Exported: stop
%%

stop(Pid) ->
    serv:call(Pid, stop).

%%
%% Server
%%

init(Parent, {IpAddress, Port}) ->
    {ok, NodisSubscription} = nodis_serv:subscribe(),
    ?LOG_INFO("Gaia NIF is initializing..."),
    ok = gaia_nif:start({#{addr_port => {inet_parse:ntoa(IpAddress), Port},
                           opus_enabled => false},
                         #{pcm_name => ?DEFAULT_PCM_NAME,
                           opus_enabled => false}}),
    ?LOG_INFO("Gaia NIF has been initialized"),
    ?LOG_INFO("Gaia server has been started"),
    {ok, #{parent => Parent,
           nodis_subscription => NodisSubscription,
           neighbours => #{}}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, NeighbourWorkers} ->
            [NetworkSenderPid] =
                supervisor_helper:get_selected_worker_pids(
                  [gaia_network_sender_serv], NeighbourWorkers),
            %% FIMXE: REMOVE!!!
            ok = gaia_network_sender_serv:update_config(
                   NetworkSenderPid,
                   #{dest_addresses => [{?DEFAULT_ADDR, ?DEFAULT_PORT}]}),
            {swap_message_handler, fun ?MODULE:message_handler/1,
             State#{network_sender_pid => NetworkSenderPid}}
    end.

message_handler(#{parent := Parent,
                  nodis_subscription := NodisSubscription,
                  neighbours := Neighbours} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            ok = gaia_nif:stop(),
            {stop, From, ok};
        {nodis, NodisSubscription, {pending, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {pending, Address}}),
            {noreply, State#{neighbours => Neighbours#{Address => pending}}};
        {nodis, NodisSubscription, {up, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {up, Address}}),
            {noreply, State#{neighbours => Neighbours#{Address => up}}};
        {nodis, NodisSubscription, {down, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {down, Address}}),
            {noreply, State#{neighbours => maps:remove(Address, Neighbours)}};
        {nodis, NodisSubscription, {wait, Address}} ->
            ?LOG_DEBUG(#{module => ?MODULE, nodis => {wait, Address}}),
            {noreply, State#{neighbours => Neighbours#{Address => wait}}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.