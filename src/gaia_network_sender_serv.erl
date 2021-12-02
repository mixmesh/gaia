-module(gaia_network_sender_serv).
-export([start_link/3, stop/0, update_config/0, update_config/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("gaia.hrl").

-define(SECONDS_BETWEEN_1970_and_2021, 1609459200).

%%
%% Exported: start_link
%%

start_link(GaiaId, SrcAddress, UseAudioSource) ->
    ?spawn_server(fun(Parent) ->
                          init(Parent, GaiaId, SrcAddress, UseAudioSource)
                  end,
                  fun initial_message_handler/1).

%%
%% Exported: stop
%%

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: update_config
%%

update_config() ->
    update_config(#{dest_adresses => [{?DEFAULT_ADDR, ?DEFAULT_PORT}]}).

update_config(Config) ->
    serv:cast(?MODULE, {update_config, Config}).

%%
%% Server
%%

init(Parent, GaiaId, {_SrcIpAddress, SrcPort} = SrcAddress, UseAudioSource) ->
    ?LOG_INFO("Gaia network sender server has been started"),
    case gen_udp:open(SrcPort, [{mode, binary}, {active, false}]) of
        {ok, Socket} ->
            {ok, #{parent => Parent,
                   gaia_id => GaiaId,
                   src_address => SrcAddress,
                   use_audio_source => UseAudioSource,
                   socket => Socket,
                   sender_pid => not_started,
                   config => not_set,
                   seqnum => 1}};
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
                  gaia_id := GaiaId,
                  src_address := _SrcAddress,
                  use_audio_source := UseAudioSource,
                  socket := Socket,
                  sender_pid := SenderPid,
                  config := Config,
                  seqnum := Seqnum} = State) ->
    receive
        {call, From, stop} ->
            exit(SenderPid, die),
            {stop, From, ok};
        {cast, {update_config, NewConfig}} when UseAudioSource ->
            case {Config, NewConfig} of
                {#{dest_addresses := DestAddresses},
                 #{dest_addresses := DestAddresses}} ->
                    {noreply, State#{config => NewConfig}};
                {_, #{dest_addresses := []}} ->
                    ok = gaia_audio_source:unsubscribe(),
                    {noreply, State#{config => NewConfig}};
                {#{dest_addresses := []}, _} ->
                    ok = gaia_audio_source:subscribe(),
                    {noreply, State#{config => NewConfig}};
                _ ->
                    {noreply, State#{config => NewConfig}}
            end;
        {cast, {update_config, NewConfig}} ->
            case {Config, NewConfig} of
                {#{dest_addresses := DestAddresses},
                 #{dest_addresses := DestAddresses}} ->
                    {noreply, State#{config => NewConfig}};
                {_, #{dest_addresses := []}} when SenderPid /= not_started ->
                    exit(SenderPid, die),
                    {noreply, State#{sender_pid => not_started,
                                     config => NewConfig}};
                {#{dest_addresses := []}, #{dest_addresses := DestAddresses}} ->
                    if
                        SenderPid /= not_started ->
                            exit(SenderPid, die);
                        true ->
                            ok
                    end,
                    NewSenderPid =
                        spawn_link(
                          fun() ->
                                  start_sender(GaiaId, Socket, DestAddresses)
                          end),
                    {noreply, State#{sender_pid => NewSenderPid,
                                     config => NewConfig}}
            end;
        {subscription_packet, Packet} ->
            #{dest_addresses := DestAddresses} = Config,
            ok = send_packet(GaiaId, Socket, Socket, Seqnum, DestAddresses,
                             Packet),
            {noreply, State#{seqnum => Seqnum + 1}};
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {'EXIT', SenderPid, normal} ->
            noreply;
        {'EXIT', SenderPid, Reason} ->
            ?LOG_ERROR(#{module => ?MODULE, exit_reason => Reason}),
            noreply;
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

%%
%% Sender
%%

start_sender(GaiaId, Socket, DestAddresses) ->
    WantedHwParams =
        #{format => ?FORMAT,
          channels => ?CHANNELS,
          rate => ?RATE_IN_HZ,
          period_size => ?PERIOD_SIZE_IN_FRAMES,
          buffer_size => ?PERIOD_SIZE_IN_FRAMES * ?BUFFER_MULTIPLICATOR},
    case alsa:open(?DEFAULT_PCM_NAME, capture, WantedHwParams, #{}) of
        {ok, AlsaHandle, _ActualHwParams, _ActualSwParams} ->
            sender_loop(GaiaId, Socket, DestAddresses, AlsaHandle, 1);
        {error, Reason} ->
            exit(alsa:strerror(Reason))
    end.

sender_loop(GaiaId, Socket, DestAddresses, AlsaHandle, Seqnum) ->
    case alsa:read(AlsaHandle, ?PERIOD_SIZE_IN_FRAMES) of
        {ok, Packet} when is_binary(Packet) ->
            ok = send_packet(GaiaId, Socket, Socket, Seqnum, DestAddresses, Packet),
            sender_loop(GaiaId, Socket, DestAddresses, AlsaHandle, Seqnum + 1);
        {ok, overrun} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => alsa:strerror(overrun)}),
            sender_loop(GaiaId, DestAddresses, Socket, AlsaHandle, Seqnum);
        {ok, suspend_event} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => alsa:strerror(suspend_event)}),
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
