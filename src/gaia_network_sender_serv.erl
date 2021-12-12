-module(gaia_network_sender_serv).
-export([start_link/4, stop/1, set_dest_addresses/2]).
-export([message_handler/1]).
-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("globals.hrl").

-define(SECONDS_BETWEEN_1970_and_2021, 1609459200).

%%
%% Exported: start_link
%%

start_link(GaiaId, BindAddress, UseCallback, OpusEnabled) ->
    ?spawn_server(
       fun(Parent) ->
               init(Parent, GaiaId, BindAddress, UseCallback, OpusEnabled)
       end,
       fun initial_message_handler/1).

%%
%% Exported: stop
%%

stop(Pid) ->
    serv:call(Pid, stop).

%%
%% Exported: set_dest_addresses
%%

set_dest_addresses(Pid, DestAddresses) ->
    serv:cast(Pid, {set_dest_addresses, DestAddresses}).

%%
%% Server
%%

init(Parent, GaiaId, {IpAddress, _Port}, UseCallback, OpusEnabled) ->
    case gen_udp:open(0, [{ifaddr, IpAddress}, {mode, binary},
                          {active, false}]) of
        {ok, Socket} ->
            ?LOG_INFO("Gaia network sender server has been started"),
            {ok, #{parent => Parent,
                   gaia_id => GaiaId,
                   use_callback => UseCallback,
                   opus_encoder => create_opus_encoder(OpusEnabled),
                   socket => Socket,
                   seqnum => 1,
                   dest_addresses => [],
                   subscription => false}};
        {error, Reason} ->
            {error, Reason}
    end.

create_opus_encoder(true) ->
    {ok, OpusEncoder} =
        opus:create_encoder(?RATE_IN_HZ, ?CHANNELS, audio, ?OPUS_COMPLEXITY),
    {opus_encoder, OpusEncoder};
create_opus_encoder(false) ->
    no_opus_encoder.

initial_message_handler(State) ->
    receive
        {neighbour_workers, NeighbourWorkers} ->
            [AudioSourcePid] =
                supervisor_helper:get_selected_worker_pids(
                  [gaia_audio_source_serv], NeighbourWorkers),
            {swap_message_handler, fun ?MODULE:message_handler/1,
             State#{audio_source_pid => AudioSourcePid}}
    end.

message_handler(#{parent := Parent,
                  audio_source_pid := AudioSourcePid,
                  gaia_id := GaiaId,
                  use_callback := UseCallback,
                  opus_encoder := OpusEncoder,
                  socket := Socket,
                  seqnum := Seqnum,
                  dest_addresses := DestAddresses} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            {stop, From, ok};
        {cast, {set_dest_addresses, NewDestAddresses}} ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         call => {set_dest_addresses, DestAddresses,
                                  NewDestAddresses}}),
            case {DestAddresses, lists:sort(NewDestAddresses)} of
                {_, DestAddresses} ->
                    noreply;
                {_, []} ->
                    _ = gaia_audio_source_serv:unsubscribe(AudioSourcePid),
                    {noreply, State#{dest_addresses => [],
                                     subscription => false}};
                {[], SortedNewDestAddresses} when UseCallback ->
                    Callback = create_callback(
                                 GaiaId, OpusEncoder, Socket, Seqnum,
                                 SortedNewDestAddresses),
                    ok = gaia_audio_source_serv:subscribe(AudioSourcePid, Callback),
                    {noreply, State#{dest_addresses => SortedNewDestAddresses}};
                {[], SortedNewDestAddresses}  ->
                    ok = gaia_audio_source_serv:subscribe(AudioSourcePid),
                    {noreply, State#{dest_addresses => SortedNewDestAddresses}};
                {_, SortedNewDestAddresses} ->
                    {noreply, State#{dest_addresses => SortedNewDestAddresses}}
            end;
        {subscription_packet, Packet} ->
            ok = send_packet(GaiaId, OpusEncoder, Socket, Seqnum, DestAddresses,
                             Packet),
            {noreply, State#{seqnum => Seqnum + 1}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

create_callback(GaiaId, OpusEncoder, Socket, Seqnum, DestAddresses) ->
    fun(Packet) when is_binary(Packet) ->
            ok = send_packet(GaiaId, OpusEncoder, Socket, Seqnum, DestAddresses,
                             Packet),
            create_callback(GaiaId, OpusEncoder, Socket, Seqnum + 1, DestAddresses);
       (OldCallback) when is_function(OldCallback) ->
            OldSeqnum = OldCallback(seqnum),
            create_callback(GaiaId, OpusEncoder, Socket, OldSeqnum, DestAddresses);
       (seqnum) ->
            Seqnum
    end.

send_packet(GaiaId, {opus_encoder, OpusEncoder}, Socket, Seqnum,
            DestAddresses, Packet) ->
    {ok, EncodedPacket} =
        opus:encode(OpusEncoder, ?OPUS_MAX_PACKET_LEN_IN_BYTES, ?CHANNELS,
                    ?SAMPLE_SIZE_IN_BYTES, Packet),
    send_packet(GaiaId, opus_encoded, Socket, Seqnum, DestAddresses,
                EncodedPacket);
send_packet(GaiaId, _OpusEncoder, Socket, Seqnum, DestAddresses, Packet) ->
    Timestamp = erlang:system_time(microsecond) -
        ?SECONDS_BETWEEN_1970_and_2021 * 1000000,
    PacketLen = size(Packet),
    Buf = <<GaiaId:32/unsigned-integer, Timestamp:64/unsigned-integer,
            Seqnum:32/unsigned-integer, PacketLen:16/unsigned-integer,
            Packet/binary>>,
    lists:foreach(
      fun({IpAddress, Port}) ->
              case gen_udp:send(Socket, IpAddress, Port, Buf) of
                  ok ->
                      ok;
                  {error, Reason} ->
                      ?LOG_ERROR(#{module => ?MODULE,
                                   function => {gen_udp, send, 4},
                                   reason => file:format_error(Reason)}),
                      ok
              end
      end, DestAddresses).
