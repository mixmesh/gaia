-module(gaia_audio_sink_serv).
-export([start_link/0, start_link/1, stop/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("kernel/include/logger.hrl").
-include("globals.hrl").

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
%% Server
%%

init(Parent, Params) ->
    AudioConsumerPid = spawn_link(fun() ->
					  audio_consumer_init(Params)
				  end),
    ?LOG_INFO("Gaia audio sink server has been started"),
    {ok, #{parent => Parent, audio_consumer_pid => AudioConsumerPid}}.

message_handler(#{parent := Parent}) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            noreply;
        {call, From, stop} ->
            ?LOG_DEBUG(#{module => ?MODULE, call => stop}),
            {stop, From, ok};
        {system, From, Request} ->
            ?LOG_DEBUG(#{module => ?MODULE, system => Request}),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?LOG_ERROR(#{module => ?MODULE, unknown_message => UnknownMessage}),
            noreply
    end.

audio_consumer_init(Params) ->
    PeriodSizeInFrames =
	proplists:get_value(period_size, Params, ?PERIOD_SIZE_IN_FRAMES),
    BufferPeriods =
	proplists:get_value(buffer_periods, Params, ?BUFFER_PERIODS),
    BufferSizeInFrames = PeriodSizeInFrames * BufferPeriods,
    Format = proplists:get_value(format, Params, ?FORMAT),
    Channels = proplists:get_value(channels, Params, ?CHANNELS),
    Rate = proplists:get_value(rate, Params, ?RATE_IN_HZ),
    Device = proplists:get_value(device, Params, ?DEFAULT_PCM_NAME),
    WantedHwParams =
        [{format, Format},
	 {channels, Channels},
	 {rate, Rate},
	 {period_size, PeriodSizeInFrames},
	 {buffer_size, BufferSizeInFrames}],
    WantedSwParams =
        [{start_threshold,
          ?start_threshold(PeriodSizeInFrames, BufferPeriods)}],
    ?LOG_DEBUG("WantedHwParams = ~w\nWantedSwParams = ~w",
               [WantedHwParams, WantedSwParams]),
    case alsa:open(Device, playback, WantedHwParams, WantedSwParams) of
        {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
            ?LOG_INFO(#{actual_hw_params => ActualHwParams,
                        actual_sw_params => ActualSwParams}),
            audio_consumer(AlsaHandle);
        {error, Reason} ->
            exit({alsa, open, alsa:strerror(Reason)})
    end.

audio_consumer(AlsaHandle) ->
    Packet = gaia_nif:read_packet(),
    case alsa:write(AlsaHandle, Packet) of
        {ok, underrun} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => underrun}),
            audio_consumer(AlsaHandle);
        {ok, suspend_event} ->
            ?LOG_WARNING(#{module => ?MODULE, reason => suspend_event}),
            audio_consumer(AlsaHandle);
        {error, Reason} ->
            alsa:close(AlsaHandle),
            exit({alsa, read, alsa:strerror(Reason)})
    end.