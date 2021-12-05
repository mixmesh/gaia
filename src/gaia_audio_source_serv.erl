-module(gaia_audio_source_serv).
-export([start_link/1, stop/1, subscribe/1, unsubscribe/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("gaia.hrl").

%%
%% Exported: start_link
%%

start_link(PcmName) ->
    ?spawn_server(fun(Parent) -> init(Parent, PcmName) end,
                  fun initial_message_handler/1).

%%
%% Exported: stop
%%

stop(Pid) ->
    serv:call(Pid, stop).

%%
%% Exported: subscribe
%%

subscribe(Pid) ->
    serv:call(Pid, {subscribe, self()}).

%%
%% Exported: unsubscribe
%%

unsubscribe(Pid) ->
    serv:call(Pid, {unsubscribe, self()}).

%%
%% Server
%%

init(Parent, PcmName) ->
    ?LOG_INFO("Gaia audio source server has been started"),
    AudioProducerPid = spawn_link(fun() -> audio_producer(PcmName) end),
    {ok, #{parent => Parent,
           audio_producer_pid => AudioProducerPid,
           subscriber_pids => []}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent,
                  audio_producer_pid := AudioProducerPid,
                  subscriber_pids := SubscriberPids} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            {stop, From, ok};
        {call, From, {subscribe, Pid}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => subscribe}),
            case lists:member(Pid, SubscriberPids) of
                true ->
                    {reply, From, {error, already_subscribed}};
                false ->
                    UpdatedSubscriberPids = [Pid|SubscriberPids],
                    AudioProducerPid ! {subscribers, UpdatedSubscriberPids},
                    _ = monitor(process, Pid),
                    {reply, From, ok,
                     State#{subscriber_pids => UpdatedSubscriberPids}}
            end;
        {call, From, {unsubscribe, Pid}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => unsubscribe}),
            case lists:member(Pid, SubscriberPids) of
                true ->
                    UpdatedSubscriberPids = lists:delete(Pid, SubscriberPids),
                    AudioProducerPid ! {subscribers, UpdatedSubscriberPids},
                    ok = demonitor(Pid),
                    {reply, From, ok,
                     State#{subscriber_pids => UpdatedSubscriberPids}};
                false ->
                    {reply, From, {error, not_subscribed}}
                end;
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', AudioProducerPid, Reason} ->
            ?LOG_DEBUG(#{module => ?MODULE, audio_producer_died => Reason}),
            noreply;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

audio_producer(PcmName) ->
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
            audio_producer(AlsaHandle, []);
        {error, Reason} ->
            exit(Reason)
    end.

audio_producer(AlsaHandle, []) ->
    receive
        {subscribers, SubscriberPids} ->
            audio_producer(AlsaHandle, SubscriberPids)
    end;
audio_producer(AlsaHandle, CurrentSubscriberPids) ->
    SubscriberPids =
        receive
            {subscribers, UpdatedSubscriberPids} ->
                UpdatedSubscriberPids
        after
            0 ->
                CurrentSubscriberPids
        end,
    case alsa:read(AlsaHandle, ?PERIOD_SIZE_IN_FRAMES) of
        {ok, Packet} when is_binary(Packet) ->
            lists:foreach(fun(Pid) ->
                                  Pid ! {subscription_packet, Packet}
                          end, SubscriberPids),
            audio_producer(AlsaHandle, SubscriberPids);
        {ok, overrun} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => overrun}),
            audio_producer(AlsaHandle, SubscriberPids);
        {ok, suspend_event} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => suspend_event}),
            audio_producer(AlsaHandle, SubscriberPids);
        {error, Reason} ->
            alsa:close(AlsaHandle),
            exit(Reason)
    end.
