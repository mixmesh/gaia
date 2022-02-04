-module(gaia_audio_sink_serv).
-export([start_link/0, start_link/1, stop/0]).
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
%% Server
%%

init(Parent, Params) ->
    AudioConsumerPid = spawn_link(fun() -> start_audio_consumer(Params) end),
    ?LOG_INFO("Gaia audio sink server has been started"),
    {ok, #{parent => Parent, audio_consumer_pid => AudioConsumerPid}}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1, State}
    end.

message_handler(#{parent := Parent, audio_consumer_pid := AudioConsumerPid}) ->
    receive
        {call, From, stop = Call} ->
            ?LOG_DEBUG(#{call => Call}),
            {stop, From, ok};
        {system, From, Request} ->
            ?LOG_DEBUG(#{system => Request}),
            {system, From, Request};
        {'EXIT', AudioConsumerPid, Reason} ->
            ?LOG_DEBUG(#{audio_consumer_died => Reason}),
            noreply;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{unknown_message => UnknownMessage}),
            noreply
    end.

start_audio_consumer(Params) ->
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
    WantedSwParams =
        [{start_threshold,
          ?start_threshold(PeriodSizeInFrames, BufferPeriods)}],
    ?LOG_DEBUG(#{wanted_hw_params => WantedHwParams,
                 wanted_sw_params => WantedSwParams}),
    AlsaHandle = force_open_alsa(PcmName, WantedHwParams, WantedSwParams),
    audio_consumer(AlsaHandle).

force_open_alsa(PcmName, WantedHwParams, WantedSwParams) ->
    case alsa:open(PcmName, playback, WantedHwParams, WantedSwParams) of
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
            force_open_alsa(PcmName, WantedHwParams, WantedSwParams)
    end.

audio_consumer(AlsaHandle) ->
    Packet = gaia_nif:read_packet(),
    case alsa:write(AlsaHandle, Packet) of
        {ok, N} when is_integer(N) ->
            audio_consumer(AlsaHandle);
        {ok, underrun} ->
            ?LOG_WARNING(#{reason => underrun}),
            audio_consumer(AlsaHandle);
        {ok, suspend_event} ->
            ?LOG_WARNING(#{reason => suspend_event}),
            audio_consumer(AlsaHandle);
        {error, Reason} ->
            ?LOG_ERROR(#{function => {alsa, read, 2},
                         reason => alsa:strerror(Reason)}),
            timer:sleep(?ALSA_PUSHBACK_TIMEOUT_IN_MS),
            audio_consumer(AlsaHandle)
    end.
