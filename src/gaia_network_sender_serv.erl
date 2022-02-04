-module(gaia_network_sender_serv).
-export([start_link/3, stop/0, set_conversation_addresses/1]).
-export([message_handler/1]).
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/bits.hrl").
-include_lib("kernel/include/logger.hrl").
-include("globals.hrl").

-define(SECONDS_BETWEEN_1970_and_2021, 1609459200).

%%
%% Exported: start_link
%%

start_link(PeerId, UseCallback, OpusEnabled) ->
    ?spawn_server(
       fun(Parent) ->
               init(Parent, PeerId, UseCallback, OpusEnabled)
       end,
       fun initial_message_handler/1,
       #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: set_conversation_addresses
%%

set_conversation_addresses(ConversationAddresses) ->
    serv:cast(?MODULE, {set_conversation_addresses, ConversationAddresses}).

%%
%% Server
%%

init(Parent, PeerId, UseCallback, OpusEnabled) ->
    case gen_udp:open(0, [{mode, binary}, {active, false}]) of
        {ok, Socket} ->
            ?LOG_INFO("Gaia network sender server has been started"),
            {ok, #{parent => Parent,
                   peer_id => PeerId,
                   use_callback => UseCallback,
                   opus_encoder => create_opus_encoder(OpusEnabled),
                   socket => Socket,
                   seqnum => 1,
                   flags => set_flags([{opus_enabled, OpusEnabled}]),
                   conversation_addresses => [],
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
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent,
                  peer_id := PeerId,
                  use_callback := UseCallback,
                  opus_encoder := OpusEncoder,
                  socket := Socket,
                  seqnum := Seqnum,
                  flags := Flags,
                  conversation_addresses := ConversationAddresses} = State) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            {stop, From, ok};
        {cast, {set_conversation_addresses, NewConversationAddresses} = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            case {ConversationAddresses, lists:sort(NewConversationAddresses)} of
                {_, ConversationAddresses} ->
                    noreply;
                {_, []} ->
                    _ = gaia_audio_source_serv:unsubscribe(),
                    {noreply, State#{conversation_addresses => [],
                                     subscription => false}};
                {_, SortedConversationAddresses} when UseCallback ->
                    Callback = create_callback(
                                 PeerId, OpusEncoder, Socket, Seqnum, Flags,
                                 SortedConversationAddresses),
                    ok = gaia_audio_source_serv:subscribe(Callback),
                    {noreply, State#{conversation_addresses =>
                                         SortedConversationAddresses}};
                {_, SortedConversationAddresses}  ->
                    ok = gaia_audio_source_serv:subscribe(),
                    {noreply, State#{conversation_addresses =>
                                         SortedConversationAddresses}}
            end;
        {subscription_packet, Packet} ->
            ok = send_packet(PeerId, OpusEncoder, Socket, Seqnum, Flags,
                             ConversationAddresses, Packet),
            {noreply, State#{seqnum => Seqnum + 1}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

set_flags(Options) ->
    set_flags(Options, 0).

set_flags([], Flags) ->
    Flags;
set_flags([{opus_enabled, true}|Rest], Flags) ->
    set_flags(Rest, ?bit_set(Flags, ?OPUS_ENABLED_FLAG));
set_flags([_|Rest], Flags) ->
    set_flags(Rest, Flags).

create_callback(PeerId, OpusEncoder, Socket, Seqnum, Flags,
                ConversationAddresses) ->
    fun(Packet) when is_binary(Packet) ->
            ok = send_packet(PeerId, OpusEncoder, Socket, Seqnum, Flags,
                             ConversationAddresses, Packet),
            create_callback(PeerId, OpusEncoder, Socket, Seqnum + 1, Flags,
                            ConversationAddresses);
       (OldCallback) when is_function(OldCallback) ->
            OldSeqnum = OldCallback(seqnum),
            create_callback(PeerId, OpusEncoder, Socket, OldSeqnum, Flags,
                            ConversationAddresses);
       (seqnum) ->
            Seqnum
    end.

send_packet(PeerId, {opus_encoder, OpusEncoder}, Socket, Seqnum, Flags,
            ConversationAddresses, Packet) ->
    {ok, EncodedPacket} =
        opus:encode(OpusEncoder, ?OPUS_MAX_PACKET_LEN_IN_BYTES, ?CHANNELS,
                    ?SAMPLE_SIZE_IN_BYTES, Packet),
    send_packet(PeerId, opus_encoded, Socket, Seqnum, Flags,
                ConversationAddresses, EncodedPacket);
send_packet(PeerId, _OpusEncoder, Socket, Seqnum, Flags, ConversationAddresses,
            Packet) ->
    Timestamp = erlang:system_time(microsecond) -
        ?SECONDS_BETWEEN_1970_and_2021 * 1000000,
    PacketLen = size(Packet),
    Buf = <<PeerId:32/unsigned-integer,
            Timestamp:64/unsigned-integer,
            Seqnum:32/unsigned-integer,
            PacketLen:16/unsigned-integer,
            Flags:8/unsigned-integer,
            Packet/binary>>,
    lists:foreach(
      fun({IpAddress, Port}) ->
              case gen_udp:send(Socket, IpAddress, Port, Buf) of
                  ok ->
                      ok;
                  {error, _Reason} ->
%                      ?LOG_ERROR(#{function => {gen_udp, send, 4},
%                                   reason => file:format_error(Reason)}),
                      ok
              end
      end, ConversationAddresses).
