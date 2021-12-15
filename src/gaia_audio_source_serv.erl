-module(gaia_audio_source_serv).
-export([start_link/0, start_link/1,
	 stop/1, subscribe/1, subscribe/2, unsubscribe/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("globals.hrl").

-define(ALSA_PUSHBACK_TIMEOUT_IN_MS, 1000).

%%
%% Exported: start_link
%%

start_link() ->
    start_link([]).

start_link(Params) ->
    ?spawn_server(fun(Parent) -> init(Parent, Params) end,
                  fun message_handler/1).

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

init(Parent, Params) ->
    AudioProducerPid = spawn_link(fun() ->
					  audio_producer_init(Params)
				  end),
    ?LOG_INFO("Gaia audio source server has been started"),
    {ok, #{parent => Parent,
           audio_producer_pid => AudioProducerPid,
           subscribers => []}}.

message_handler(#{parent := Parent,
                  audio_producer_pid := AudioProducerPid,
                  subscribers := Subscribers} = State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            noreply;
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            {stop, From, ok};
        {call, From, {subscribe, Pid, Callback}} ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         call => subscribe,
                         pid => Pid,
                         subscribers => Subscribers}),
            case lists:keytake(Pid, 1, Subscribers) of
                {value, {Pid, MonitorRef, _OldCallback}, PurgedSubscribers} ->
                    UpdatedSubscribers =
                        [{Pid, MonitorRef, Callback}|PurgedSubscribers],
                    AudioProducerPid ! {subscribers, UpdatedSubscribers},
                    {reply, From, ok,
                     State#{subscribers => UpdatedSubscribers}};
                false ->
                    MonitorRef = monitor(process, Pid),
                    UpdatedSubscribers =
                        [{Pid, MonitorRef, Callback}|Subscribers],
                    AudioProducerPid ! {subscribers, UpdatedSubscribers},
                    {reply, From, ok,
                     State#{subscribers => UpdatedSubscribers}}
            end;
        {call, From, {unsubscribe, Pid}} ->
            ?LOG_DEBUG(#{module => ?MODULE,
                         call => unsubscribe,
                         pid => Pid,
                         subscribers => Subscribers}),
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

audio_producer_init(Params) ->
    PeriodSizeInFrames =
	proplists:get_value(period_size, Params, ?PERIOD_SIZE_IN_FRAMES),
    BufferPeriods =
	proplists:get_value(buffer_periods, Params, ?BUFFER_PERIODS),
    BufferSizeInFrames = PeriodSizeInFrames * BufferPeriods,
    Format = proplists:get_value(format, Params, ?FORMAT),
    Channels = proplists:get_value(channels, Params, ?CHANNELS),
    Rate = proplists:get_value(rate, Params, ?RATE_IN_HZ),
    PcmName = proplists:get_value(device, Params, ?DEFAULT_PCM_NAME),
    WantedHwParams =
        [{format, Format},
	 {channels, Channels},
	 {rate, Rate},
	 {period_size, PeriodSizeInFrames},
	 {buffer_size, BufferSizeInFrames}],
    ?LOG_DEBUG(#{module => ?MODULE, wanted_hw_params => WantedHwParams}),
    AlsaHandle = force_open_alsa(PcmName, WantedHwParams),
    audio_producer(AlsaHandle, PeriodSizeInFrames, []).

force_open_alsa(PcmName, WantedHwParams) ->
    case alsa:open(PcmName, capture, WantedHwParams, []) of
        {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
            ?LOG_INFO(#{module => ?MODULE,
                        actual_hw_params => ActualHwParams,
                        actual_sw_params => ActualSwParams}),
            %% Ensure that period size is exact or else things will break
            {value, {_, WantedPeriodSizeInFrames}} =
                lists:keysearch(period_size, 1, WantedHwParams),
            {value, {_, WantedPeriodSizeInFrames}} =
                lists:keysearch(period_size, 1, ActualHwParams),
            AlsaHandle;
        {error, Reason} ->
            ?LOG_ERROR(#{module => ?MODULE,
                         function => {alsa, open, 3},
                         reason => alsa:strerror(Reason)}),
            timer:sleep(?ALSA_PUSHBACK_TIMEOUT_IN_MS),
            force_open_alsa(PcmName, WantedHwParams)
    end.

audio_producer(AlsaHandle, PeriodSizeInFrames, []) ->
    receive
        {subscribers, Subscribers} ->
            audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers)
    end;
audio_producer(AlsaHandle, PeriodSizeInFrames, CurrentSubscribers) ->
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
    case alsa:read(AlsaHandle, PeriodSizeInFrames) of
        {ok, Packet} when is_binary(Packet) ->
            MergedSubscribers =
                lists:map(fun({Pid, _MonitorRef, bang} = Subscriber) ->
                                  Pid ! {subscription_packet, Packet},
                                  Subscriber;
                             ({Pid, MonitorRef, Callback}) ->
                                  NextCallback = Callback(Packet),
                                  {Pid, MonitorRef, NextCallback}
                          end, Subscribers),
            audio_producer(AlsaHandle, PeriodSizeInFrames, MergedSubscribers);
        {ok, overrun} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => overrun}),
            audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers);
        {ok, suspend_event} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => suspend_event}),
            audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers);
        {error, Reason} ->
            ?LOG_ERROR(#{module => ?MODULE,
                         function => {alsa, read, 2},
                         reason => alsa:strerror(Reason)}),
            timer:sleep(?ALSA_PUSHBACK_TIMEOUT_IN_MS),
            audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers)
    end.
