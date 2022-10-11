-module(gaia_audio_source_serv).
-export([start_link/0, start_link/1,
	 stop/0, subscribe/0, subscribe/1, unsubscribe/0,
         trigger_callback/1,
         serve_only_me/0, serve_all/0]).
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
                  fun initial_message_handler/1,
                  #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: subscribe
%%

subscribe(Callback) ->
    serv:call(?MODULE, {subscribe, self(), Callback}).

subscribe() ->
    serv:call(?MODULE, {subscribe, self(), bang}).

%%
%% Exported: unsubscribe
%%

unsubscribe() ->
    serv:call(?MODULE, {unsubscribe, self()}).

%%
%% Exported: trigger_callback
%%

trigger_callback(Term) ->
    serv:cast(?MODULE, {trigger_callback, self(), Term}).

%%
%% Exported: serve_only_me
%%

serve_only_me() ->
    serv:cast(?MODULE, {serve_only_me, self()}).

%%
%% Exported: serve_all
%%

serve_all() ->
    serv:cast(?MODULE, serve_all).

%%
%% Server
%%

init(Parent, Params) ->
    AudioProducerPid =
        spawn_link(fun() -> audio_producer_init(Params) end),
    ?LOG_INFO("Gaia audio source server has been started (~w)",
              [serv:since_system_start()]),
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
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            {stop, From, ok};
        {call, From, {subscribe, Pid, NewCallback} = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            case lists:keytake(Pid, 1, Subscribers) of
                {value, {Pid, MonitorRef, _Callback}, PurgedSubscribers} ->
                    UpdatedSubscribers =
                        [{Pid, MonitorRef, NewCallback}|PurgedSubscribers],
                    AudioProducerPid ! {subscribers, UpdatedSubscribers},
                    {reply, From, ok,
                     State#{subscribers => UpdatedSubscribers}};
                false ->
                    MonitorRef = monitor(process, Pid),
                    UpdatedSubscribers =
                        [{Pid, MonitorRef, NewCallback}|Subscribers],
                    AudioProducerPid ! {subscribers, UpdatedSubscribers},
                    {reply, From, ok,
                     State#{subscribers => UpdatedSubscribers}}
            end;
        {call, From, {unsubscribe, Pid} = Call} ->
            ?LOG_DEBUG(#{call => Call}),
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
        {cast, {trigger_callback, Pid, Term} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            case lists:keytake(Pid, 1, Subscribers) of
                {value, {Pid, MonitorRef, Callback}, PurgedSubscribers} ->
                    NewCallback = Callback(Term),
                    UpdatedSubscribers =
                        [{Pid, MonitorRef, NewCallback}|PurgedSubscribers],
                    AudioProducerPid ! {subscribers, UpdatedSubscribers},
                    {noreply, State#{subscribers => UpdatedSubscribers}};
                false ->
                    noreply
            end;
        {cast, {serve_only_me, Pid} = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            AudioProducerPid ! {serve_only_me, Pid},
            noreply;
        {cast, serve_all = Cast} ->
            ?LOG_DEBUG(#{cast => Cast}),
            AudioProducerPid ! serve_all,
            noreply;
        {'DOWN', _Ref, process, Pid, Info} ->
            ?LOG_DEBUG(#{subscriber_down => Info}),
            UpdatedSubscribers = lists:keydelete(Pid, 1, Subscribers),
            AudioProducerPid ! {subscribers, UpdatedSubscribers},
            {noreply, State#{subscribers => UpdatedSubscribers}};
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {'EXIT', AudioProducerPid, Reason} ->
            ?LOG_DEBUG(#{audio_producer_died => Reason}),
            noreply;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
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
    ?LOG_DEBUG(#{wanted_hw_params => WantedHwParams}),
    AlsaHandle = force_open_alsa(PcmName, WantedHwParams),
    audio_producer(AlsaHandle, PeriodSizeInFrames, [], all).

force_open_alsa(PcmName, WantedHwParams) ->
    case alsa:open(PcmName, capture, WantedHwParams, []) of
        {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
            ?LOG_DEBUG(#{actual_hw_params => ActualHwParams,
                         actual_sw_params => ActualSwParams}),
            %% Ensure that period size is exact or else things will break
            {value, {_, WantedPeriodSizeInFrames}} =
                lists:keysearch(period_size, 1, WantedHwParams),
            {value, {_, WantedPeriodSizeInFrames}} =
                lists:keysearch(period_size, 1, ActualHwParams),
            AlsaHandle;
        {error, Reason} ->
            ?LOG_ERROR(#{function => {alsa, open, 3},
                         reason => alsa:strerror(Reason)}),
            timer:sleep(?ALSA_PUSHBACK_TIMEOUT_IN_MS),
            force_open_alsa(PcmName, WantedHwParams)
    end.

audio_producer(AlsaHandle, PeriodSizeInFrames, [], CurrentServeWho) ->
    receive
        {subscribers, Subscribers} ->
            audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers,
                           CurrentServeWho)
    end;
audio_producer(AlsaHandle, PeriodSizeInFrames, CurrentSubscribers,
               CurrentServeWho) ->
    case handle_audio_producer_commands(CurrentSubscribers, CurrentServeWho) of
        {[], ServeWho} ->
            audio_producer(AlsaHandle, PeriodSizeInFrames, [], ServeWho);
        {Subscribers, ServeWho} ->
            case alsa:read(AlsaHandle, PeriodSizeInFrames) of
                {ok, Packet} when is_binary(Packet) ->
                    MergedSubscribers =
                        lists:map(
                          fun({Pid, _MonitorRef, bang} = Subscriber)
                                when Pid == ServeWho orelse ServeWho == all ->
                                  Pid ! {subscription_packet, Packet},
                                  Subscriber;
                             ({Pid, MonitorRef, Callback} = Subscriber)
                                when Pid == ServeWho orelse ServeWho == all ->
                                  try Callback(Packet) of
                                      NewCallback ->
                                          {Pid, MonitorRef, NewCallback}
                                  catch
                                      _:Reason ->
                                          ?LOG_ERROR(
                                             #{callback_failure => Reason}),
                                          Subscriber
                                  end;
                             (Subscriber) ->
                                  Subscriber
                          end, Subscribers),
                    audio_producer(AlsaHandle, PeriodSizeInFrames,
                                   MergedSubscribers, ServeWho);
                {ok, overrun} ->
                    ?LOG_WARNING(#{reason => overrun}),
                    audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers,
                                   ServeWho);
                {ok, suspend_event} ->
                    ?LOG_WARNING(#{reason => suspend_event}),
                    audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers,
                                   ServeWho);
                {error, Reason} ->
                    ?LOG_ERROR(#{function => {alsa, read, 2},
                                 reason => alsa:strerror(Reason)}),
                    timer:sleep(?ALSA_PUSHBACK_TIMEOUT_IN_MS),
                    audio_producer(AlsaHandle, PeriodSizeInFrames, Subscribers,
                                   ServeWho)
            end
    end.

handle_audio_producer_commands(CurrentSubscribers, CurrentServeWho) ->
    receive
        {subscribers, UpdatedSubscribers} = Command ->
            ?LOG_DEBUG(#{audio_producer_command => Command}),
            Subscribers =
                lists:map(
                  fun({Pid, MonitorRef, NewCallback} = Subscriber) ->
                          case lists:keysearch(Pid, 1, CurrentSubscribers) of
                              {value, {Pid, MonitorRef, bang}} ->
                                  Subscriber;
                              {value, {Pid, MonitorRef, Callback}} ->
                                  %% NOTE: This used by the callback in
                                  %% gaia_network_sernder_serv.erl to reuse
                                  %% an already running seqnum
                                  {Pid, MonitorRef, NewCallback(Callback)};
                              false ->
                                  Subscriber
                          end
                  end, UpdatedSubscribers),
            handle_audio_producer_commands(Subscribers, CurrentServeWho);
        {serve_only_me, Pid} = Command ->
            ?LOG_DEBUG(#{audio_producer_command => Command}),
            handle_audio_producer_commands(CurrentSubscribers, Pid);
        serve_all = Command ->
            ?LOG_DEBUG(#{audio_producer_command => Command}),
            handle_audio_producer_commands(CurrentSubscribers, all)
    after
        0 ->
            {CurrentSubscribers, CurrentServeWho}
    end.
