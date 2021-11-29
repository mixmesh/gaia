-module(gaia_network_sender).
-export([start_link/0, stop/0]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").
-include("gaia.hrl").

-define(SECONDS_BETWEEN_1970_and_2021, 1609459200).

%%
%% Exported: start_link
%%

start_link() ->
    ?spawn_server_opts(fun init/1,
                       fun ?MODULE:message_handler/1,
                       #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

stop() ->
    serv:call(?MODULE, stop).

%%
%% Server
%%

init(Parent) ->
    ?daemon_log_tag_fmt(system, "Network receiver server has been started", []),
    Sender =
        spawn_link(
          fun() ->
                  start_sender(1, ?DEFAULT_PORT + 1,
                               [{?DEFAULT_ADDR, ?DEFAULT_PORT}])
          end),
    {ok, #{parent => Parent, sender => Sender}}.

message_handler(#{parent := Parent, sender := Sender} = State) ->
    receive
        {call, From, stop} ->
            exit(Sender, kill),
            {stop, From, ok};
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            case Pid of
                Sender ->
                    io:format(standard_error,"The sender died: ~p\n", [Reason]),
                    stop;
                _ ->
                    noreply
            end;
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

%%
%% Sender
%%

start_sender(Userid, SrcPort, DestAddresses) ->
    case gen_udp:open(SrcPort, [{mode, binary}, {active, false}]) of
        {ok, Socket} ->
            WantedHwParams =
                #{format => ?FORMAT,
                  channels => ?CHANNELS,
                  rate => ?RATE_IN_HZ,
                  period_size => ?PERIOD_SIZE_IN_FRAMES,
                  buffer_size =>
                      ?PERIOD_SIZE_IN_FRAMES * ?BUFFER_MULTIPLICATOR},
            case alsa:open(?DEFAULT_PCM_NAME, capture, WantedHwParams, #{}) of
                {ok, AlsaHandle, _ActualHwParams, _ActualSwParams} ->
                    sender_loop(Userid, DestAddresses, Socket, AlsaHandle, 1);
                {error, Reason} ->
                    exit({error, alsa:strerror(Reason)})
            end;
        {error, Reason} ->
            exit({error, file:format_error(Reason)})
    end.

sender_loop(Userid, DestAddresses, Socket, AlsaHandle, Seqnum) ->
    case alsa:read(AlsaHandle, ?PERIOD_SIZE_IN_FRAMES) of
        {ok, Packet} when is_binary(Packet) ->
            Timestamp = erlang:system_time(microsecond) -
                ?SECONDS_BETWEEN_1970_and_2021 * 1000000,
            PacketLen = size(Packet),
            Buf = <<Userid:32/unsigned-integer, Timestamp:64/unsigned-integer,
                    Seqnum:32/unsigned-integer, PacketLen:16/unsigned-integer,
                    Packet/binary>>,
            lists:foreach(
              fun({IpAddress, Port}) ->
                      case gen_udp:send(Socket, IpAddress, Port, Buf) of
                          ok ->
                              ok;
                          {error, Reason} ->
                              exit({error,
                                    io:format(standard_error,"~s\n",
                                              [file:format_error(Reason)])})
                      end
              end, DestAddresses),
            sender_loop(Userid, DestAddresses, Socket, AlsaHandle, Seqnum + 1);
        {ok, overrun} ->
            io:format(standard_error, "~s\n", [alsa:strerror(overrun)]),
            sender_loop(Userid, DestAddresses, Socket, AlsaHandle, Seqnum);
        {ok, suspend_event} ->
            io:format(standard_error, "~s\n", [alsa:strerror(suspend_event)]),
            sender_loop(Userid, DestAddresses, Socket, AlsaHandle, Seqnum);
        {error, Reason} ->
            alsa:close(AlsaHandle),
            exit({error, alsa:strerror(Reason)})
    end.
