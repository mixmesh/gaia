-ifndef(GLOBALS_HRL).
-define(GLOBALS_HRL, true).

%% NOTE: This file *must* be kept in harmony with ../c_src/globals.h

-define(DEFAULT_PCM_NAME, "plughw:0,0").
-define(DEFAULT_ADDR, {127, 0, 0, 1}).
-define(DEFAULT_ADDR_STRING, "127.0.0.1").
-define(DEFAULT_PORT, 2305).

-define(FORMAT, s16_le).
-define(CHANNELS, 2).
-define(SAMPLE_SIZE_IN_BYTES, 2).
-define(FRAME_SIZE_IN_BYTES, (?CHANNELS * ?SAMPLE_SIZE_IN_BYTES)).
-define(RATE_IN_HZ, 48000).

-define(PERIOD_SIZE_IN_FRAMES, 960).
-define(PERIOD_SIZE_IN_BYTES, (?PERIOD_SIZE_IN_FRAMES * ?FRAME_SIZE_IN_BYTES)).
-define(PERIOD_SIZE_IN_MS, (?PERIOD_SIZE_IN_FRAMES / (?RATE_IN_HZ / 1000.0))).
-define(BUFFER_PERIODS, 8).
-define(start_threshold(PeriodSizeInFrames, BufferPeriods),
        (PeriodSizeInFrames * (BufferPeriods - 1))).

-endif.
