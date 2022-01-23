-module(test42).
-compile(export_all).

%%-define(CAPTURE_PCM_NAME, "bluealsa:DEV=20:74:CF:C5:28:79,PROFILE=sco").
%%-define(PLAYBACK_PCM_NAME, "bluealsa:DEV=20:74:CF:C5:28:79,PROFILE=a2dp").
-define(CAPTURE_PCM_NAME, "default").
-define(PLAYBACK_PCM_NAME, "default").
-define(FORMAT, s16_le).
-define(CHANNELS, 1).
-define(SAMPLE_SIZE_IN_BYTES, 2).
-define(FRAME_SIZE_IN_BYTES, (?CHANNELS * ?SAMPLE_SIZE_IN_BYTES)).
-define(RATE, 16000).
-define(PERIOD_SIZE_IN_MS, 20).
-define(PERIOD_SIZE_IN_FRAMES, trunc(?PERIOD_SIZE_IN_MS / 1000.0 * ?RATE)).
-define(PERIOD_SIZE_IN_BYTES, (?PERIOD_SIZE_IN_FRAMES * ?FRAME_SIZE_IN_BYTES)).
-define(BUFFER_PERIODS, 8).
-define(BUFFER_SIZE_IN_FRAMES, (?PERIOD_SIZE_IN_FRAMES * ?BUFFER_PERIODS)).
-define(start_threshold(PeriodSizeInFrames, BufferPeriods),
        (PeriodSizeInFrames * (BufferPeriods - 1))).

start() ->
    spawn(fun producer/0),
    spawn(fun consumer/0).

producer() ->
    WantedHwParams =
        [{format, ?FORMAT},
	 {channels, ?CHANNELS},
	 {rate, ?RATE},
	 {period_size, ?PERIOD_SIZE_IN_FRAMES},
	 {buffer_size, ?BUFFER_SIZE_IN_FRAMES}],
    case alsa:open(?CAPTURE_PCM_NAME, capture, WantedHwParams, []) of
        {ok, H, _ActualHwParams, _ActualSwParams} ->
            producer(H);
        {error, Reason} ->
            io:format("~s", [alsa:strerror(Reason)])
    end.

producer(H) ->
    case alsa:read(H, ?PERIOD_SIZE_IN_FRAMES) of
        {ok, Packet} when is_binary(Packet) ->
            io:format("+"),
            producer(H);
        {ok, overrun} ->
            io:format("o"),
            producer(H);
        {ok, suspend_event} ->
            io:format("s"),
            producer(H);
        {error, Reason} ->
            alsa:close(H),
            io:format("~s", [alsa:strerror(Reason)])
    end.

consumer() ->
    WantedHwParams =
        [{format, ?FORMAT},
	 {channels, ?CHANNELS},
	 {rate, ?RATE},
	 {period_size, ?PERIOD_SIZE_IN_FRAMES},
	 {buffer_size, ?BUFFER_SIZE_IN_FRAMES}],
    WantedSwParams =
        [{start_threshold,
          ?start_threshold(?PERIOD_SIZE_IN_FRAMES, ?BUFFER_PERIODS)}],
    case alsa:open(?PLAYBACK_PCM_NAME, playback, WantedHwParams,
                   WantedSwParams) of
        {ok, H, _ActualHwParams, _ActualSwParams} ->
            consumer(H);
        {error, Reason} ->
            io:format("~s", [alsa:strerror(Reason)])
    end.

consumer(H) ->
    Packet = crypto:strong_rand_bytes(?PERIOD_SIZE_IN_BYTES),
    case alsa:write(H, Packet) of
        {ok, N} when is_integer(N) ->
            io:format("-"),
            consumer(H);
        {ok, underrun} ->
            io:format("u"),
            consumer(H);
        {ok, suspend_event} ->
            io:format("s"),
            consumer(H);
        {error, Reason} ->
            alsa:close(H),
            io:format("~s", [alsa:strerror(Reason)])
    end.
