-ifndef(GLOBALS_HRL).
-define(GLOBALS_HRL, true).

%% NOTE: This file *must* be kept in harmony with ../c_src/globals.h

-define(DEFAULT_PCM_NAME, "plughw:0,0").
-define(DEFAULT_ADDR, {127, 0, 0, 1}).
-define(DEFAULT_ADDR_STRING, "127.0.0.1").
-define(DEFAULT_PORT, 2305).

-define(FORMAT, s16_le).
-define(CHANNELS, 1).
-define(SAMPLE_SIZE_IN_BYTES, 2).
-define(FRAME_SIZE_IN_BYTES, (?CHANNELS * ?SAMPLE_SIZE_IN_BYTES)).
-define(RATE_IN_HZ, 16000).

-define(PERIOD_SIZE_IN_MS, 20).
-define(PERIOD_SIZE_IN_FRAMES,
        trunc(?PERIOD_SIZE_IN_MS / 1000.0 * ?RATE_IN_HZ)).
-define(PERIOD_SIZE_IN_BYTES, (?PERIOD_SIZE_IN_FRAMES * ?FRAME_SIZE_IN_BYTES)).

-define(BUFFER_PERIODS, 8).
-define(start_threshold(PeriodSizeInFrames, BufferPeriods),
        (PeriodSizeInFrames * (BufferPeriods - 1))).

-define(OPUS_COMPLEXITY, 5).
-define(OPUS_MAX_PACKET_LEN_IN_BYTES, 1276).

-define(PLAYBACK_AUDIO, false).

%% |peer_id:4|timestamp:8|seqnum:4|packet_len:2|flags:1| = 19 bytes
-define(HEADER_SIZE, (4 + 8 + 4 + 2 + 1)).

-define(OPUS_ENABLED_FLAG, (1 bsl 0)).

-endif.
