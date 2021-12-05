-module(gaia_network_sender_serv).
-export([start_link/4, stop/1, set_dest_addresses/2]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("gaia.hrl").

-define(SECONDS_BETWEEN_1970_and_2021, 1609459200).

%%
%% Exported: start_link
%%

start_link(GaiaId, BindAddress, PcmName, UseAudioSource) ->
    ?spawn_server(
       fun(Parent) ->
               init(Parent, GaiaId, BindAddress, PcmName, UseAudioSource)
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

init(Parent, GaiaId, {IpAddress, _Port}, PcmName, UseAudioSource) ->
    case gen_udp:open(0, [{ifaddr, IpAddress}, {mode, binary},
                          {active, false}]) of
        {ok, Socket} ->
            ?LOG_INFO("Gaia network sender server has been started"),
            {ok, #{parent => Parent,
                   gaia_id => GaiaId,
                   pcm_name => PcmName,
                   use_audio_source => UseAudioSource,
                   socket => Socket,
                   sender_pid => not_started,
                   dest_addresses => [],
                   seqnum => 1,
                   subscription => false,
                   sender_alsa_handle => not_set}};
        {error, Reason} ->
            {error, Reason}
    end.

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
                  pcm_name := PcmName,
                  use_audio_source := UseAudioSource,
                  socket := Socket,
                  sender_pid := SenderPid,
                  dest_addresses := DestAddresses,
                  seqnum := Seqnum,
                  sender_alsa_handle := SenderAlsaHandle} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            true = kill_sender(SenderPid, SenderAlsaHandle),
            {stop, From, ok};
        {cast, {set_dest_addresses, NewDestAddresses}} when UseAudioSource ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         call => {set_dest_addresses, DestAddresses, NewDestAddresses}}),
            case {DestAddresses, lists:sort(NewDestAddresses)} of
                {_, DestAddresses} ->
                    noreply;
                {_, []} ->
                    ok = gaia_audio_source_serv:unsubscribe(AudioSourcePid),
                    {noreply, State#{dest_addresses => [],
                                     subscription => false}};
                {[], SortedNewDestAddresses} ->
                    ok = gaia_audio_source_serv:unsubscribe(AudioSourcePid),
                    {noreply, State#{dest_addresses => SortedNewDestAddresses}};
                {_, SortedNewDestAddresses} ->
                    {noreply, State#{dest_addresses => SortedNewDestAddresses}}
            end;
        {cast, {set_dest_addresses, NewDestAddresses}} ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         call => {set_dest_addresses, DestAddresses, NewDestAddresses}}),
            case {DestAddresses, lists:sort(NewDestAddresses)} of
                {_, DestAddresses} ->
                    noreply;
                {_, []} ->
                    ?LOG_DEBUG(#{module => ?MODULE, event => killing_sender}),
                    true = kill_sender(SenderPid, SenderAlsaHandle),
                    {noreply, State#{dest_addresses => []}};
                {[], SortedNewDestAddresses} ->
                    NewSenderPid =
                        start_sender(GaiaId, PcmName, Socket, SenderPid,
                                     SortedNewDestAddresses),
                    {noreply, State#{sender_pid => NewSenderPid,
                                     dest_addresses => SortedNewDestAddresses}};
                {_, SortedNewDestAddresses} ->
                    NewSenderPid =
                        start_sender(GaiaId, PcmName, Socket, SenderPid,
                                     SortedNewDestAddresses),
                    {noreply, State#{sender_pid => NewSenderPid,
                                     dest_addresses => SortedNewDestAddresses}}
            end;
        {subscription_packet, Packet} ->
            ok = send_packet(GaiaId, Socket, Socket, Seqnum, DestAddresses,
                             Packet),
            {noreply, State#{seqnum => Seqnum + 1}};
        {sender_alsa_handle, AlsaHandle} ->
            {noreply, State#{sender_alsa_handle => AlsaHandle}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            true = kill_sender(SenderPid, SenderAlsaHandle),
            exit(Reason);
        {'EXIT', SenderPid, killed} ->
            ?LOG_DEBUG(#{module => ?MODULE, exit_reason => sender_killed}),
            noreply;
        {'EXIT', SenderPid, Reason} ->
            ?LOG_ERROR(#{module => ?MODULE, exit_reason => Reason}),
            {noreply, State#{sender_pid => not_started}};
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

kill_sender(not_started, _SenderAlsaHandle) ->
    true;
kill_sender(SenderPid, not_set) ->
    exit(SenderPid, kill);
kill_sender(SenderPid, SenderAlsaHandle) ->
    alsa:close(SenderAlsaHandle),
    exit(SenderPid, kill).

start_sender(GaiaId, PcmName, Socket, SenderPid, DestAddresses) ->
    ?LOG_DEBUG(#{module => ?MODULE, event => starting_sender}),
    true = kill_sender(SenderPid, not_set),
    Parent = self(),
    spawn_link(
      fun() -> sender(Parent, GaiaId, PcmName, Socket, DestAddresses) end).

%%
%% Sender
%%

sender(Parent, GaiaId, PcmName, Socket, DestAddresses) ->
    WantedHwParams =
        #{format => ?FORMAT,
          channels => ?CHANNELS,
          sample_rate => ?RATE_IN_HZ,
          period_size => ?PERIOD_SIZE_IN_FRAMES
%          NOTE: For some reason it is not allowed to set the buffer size on PI
%          buffer_size => ?PERIOD_SIZE_IN_FRAMES * ?BUFFER_MULTIPLICATOR
         },
    case alsa:open(PcmName, capture, WantedHwParams, #{}) of
        {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
            ?LOG_INFO(#{actual_hw_params => ActualHwParams,
                        actual_sw_params => ActualSwParams}),
            Parent ! {sender_alsa_handle, AlsaHandle},
            sender_loop(GaiaId, Socket, DestAddresses, AlsaHandle, 1);
        {error, Reason} ->
            exit(alsa:strerror(Reason))
    end.

sender_loop(GaiaId, Socket, DestAddresses, AlsaHandle, Seqnum) ->
    case alsa:read(AlsaHandle, ?PERIOD_SIZE_IN_FRAMES) of
        {ok, Packet} when is_binary(Packet) ->
            ok = send_packet(GaiaId, Socket, Socket, Seqnum, DestAddresses,
                             Packet),
            sender_loop(GaiaId, Socket, DestAddresses, AlsaHandle, Seqnum + 1);
        {ok, overrun} ->
            ?LOG_WARNING(#{module => ?MODULE,
                           reason => alsa:strerror(overrun)}),
            sender_loop(GaiaId, DestAddresses, Socket, AlsaHandle, Seqnum);
        {ok, suspend_event} ->
            ?LOG_WARNING(#{module => ?MODULE,
                           reason => alsa:strerror(suspend_event)}),
            sender_loop(GaiaId, DestAddresses, Socket, AlsaHandle, Seqnum);
        {error, Reason} ->
            alsa:close(AlsaHandle),
            exit(alsa:strerror(Reason))
    end.

send_packet(GaiaId, Socket, Socket, Seqnum, DestAddresses, Packet) ->
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
                      exit(io:format(standard_error,"~s\n",
                                     [file:format_error(Reason)]))
              end
      end, DestAddresses).
