-module(gaia_audio_source_serv).
-export([start_link/0, start_link/1,
	 stop/1, subscribe/1, subscribe/2, unsubscribe/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("gaia.hrl").

%% default values
-define(DEFAULT_FORMAT,        s16_le).
-define(DEFAULT_RATE,          48000).
-define(DEFAULT_CHANNELS,      2).
-define(PERIOD_SIZE_IN_FRAMES, 4800).  %% 100ms
-define(BUFFER_PERIODS,        8).
-define(DEFAULT_DEVICE,        "plughw:0,0").

%%
%% Exported: start_link
%%

start_link() ->
    start_link([]).

start_link(Params) ->
    ?spawn_server(fun(Parent) -> init(Parent, Params) end,
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

init(Parent, Params) ->
    ?LOG_INFO("Gaia audio source server has been started"),
    AudioProducerPid = spawn_link(fun() ->
					  audio_producer_init(Params)
				  end),
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

audio_producer_init(Params) ->
    PeriodSizeInFrames =
	proplists:get_value(period_size, Params, ?PERIOD_SIZE_IN_FRAMES),
    NumBufferPeriods =
	proplists:get_value(buffer_periods, Params, ?BUFFER_PERIODS),
    BufferSizeInFrames = PeriodSizeInFrames * NumBufferPeriods,
    Format = proplists:get_value(format, Params, ?DEFAULT_FORMAT),
    Channels = proplists:get_value(channels, Params, ?DEFAULT_CHANNELS),
    Rate = proplists:get_value(rate, Params, ?DEFAULT_RATE),
    Device = proplists:get_value(device, Params, ?DEFAULT_DEVICE),
    WantedHwParams =
        [{format, Format},
	 {channels, Channels},
	 {rate, Rate},
	 {period_size, PeriodSizeInFrames},
	 {buffer_size, BufferSizeInFrames}],
    ?LOG_DEBUG("WantedHwParams = ~w", [WantedHwParams]),
    case alsa:open(Device, capture, WantedHwParams, []) of
        {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
            ?LOG_INFO(#{actual_hw_params => ActualHwParams,
                        actual_sw_params => ActualSwParams}),
            audio_producer(AlsaHandle, PeriodSizeInFrames, []);
        {error, Reason} ->
            exit(Reason)
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
            alsa:close(AlsaHandle),
            exit(Reason)
    end.
