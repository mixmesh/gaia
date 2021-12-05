-module(gaia_audio_source_serv).
-export([start_link/1, stop/1, subscribe/1, subscribe/2, unsubscribe/1]).
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

subscribe(Pid, Callback) ->
    serv:call(Pid, {subscribe, self(), Callback}).

subscribe(Pid) ->
    serv:call(Pid, {subscribe, self(), bang}).

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
           subscribers => []}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent,
                  audio_producer_pid := AudioProducerPid,
                  subscribers := Subscribers} = State) ->
    receive
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            {stop, From, ok};
        {call, From, {subscribe, Pid, Callback}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => subscribe}),
            case lists:keymember(Pid, 1, Subscribers) of
                true ->
                    {reply, From, {error, already_subscribed}};
                false ->
                    MonitorRef = monitor(process, Pid),
                    UpdatedSubscribers =
                        [{Pid, MonitorRef, Callback}|Subscribers],
                    AudioProducerPid ! {subscribers, UpdatedSubscribers},
                    {reply, From, ok,
                     State#{subscribers => UpdatedSubscribers}}
            end;
        {call, From, {unsubscribe, Pid}} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => unsubscribe}),
            case lists:keysearch(Pid, 1, Subscribers) of
                {value, {_Pid, MonitorRef, _Callback}} ->
                    UpdatedSubscribers = lists:keydelete(Pid, 1, Subscribers),
                    AudioProducerPid ! {subscribers, UpdatedSubscribers},
                    true = demonitor(MonitorRef),
                    {reply, From, ok,
                     State#{subscribers => UpdatedSubscribers}};
                false ->
                    {reply, From, {error, not_subscribed}}
            end;
        {'DOWN', _Ref, process, Pid, _Info} ->
            ?LOG_DEBUG(#{module => ?MODULE, event => subscriber_down}),
            UpdatedSubscribers = lists:keydelete(Pid, 1, Subscribers),
            AudioProducerPid ! {subscribers, UpdatedSubscribers},
            {noreply, State#{subscribers => UpdatedSubscribers}};
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
        {subscribers, Subscribers} ->
            audio_producer(AlsaHandle, Subscribers)
    end;
audio_producer(AlsaHandle, CurrentSubscribers) ->
    Subscribers =
        receive
            {subscribers, UpdatedSubscribers} ->
                lists:map(
                  fun({Pid, MonitorRef, Callback} = Subscriber) ->
                          case lists:keysearch(Pid, 1, CurrentSubscribers) of
                              {value, {Pid, MonitorRef, OldCallback}} ->
                                  {Pid, MonitorRef, Callback(OldCallback)};
                              false ->
                                  Subscriber
                          end
                  end, UpdatedSubscribers)
        after
            0 ->
                CurrentSubscribers
        end,
    case alsa:read(AlsaHandle, ?PERIOD_SIZE_IN_FRAMES) of
        {ok, Packet} when is_binary(Packet) ->
            MergedSubscribers =
                lists:map(fun({Pid, _MonitorRef, bang} = Subscriber) ->
                                  Pid ! {subscription_packet, Packet},
                                  Subscriber;
                             ({Pid, MonitorRef, Callback}) ->
                                  NextCallback = Callback(Packet),
                                  {Pid, MonitorRef, NextCallback}
                          end, Subscribers),
            audio_producer(AlsaHandle, MergedSubscribers);
        {ok, overrun} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => overrun}),
            audio_producer(AlsaHandle, Subscribers);
        {ok, suspend_event} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => suspend_event}),
            audio_producer(AlsaHandle, Subscribers);
        {error, Reason} ->
            alsa:close(AlsaHandle),
            exit(Reason)
    end.
