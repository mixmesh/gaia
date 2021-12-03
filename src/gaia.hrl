-ifndef(GAIA_HRL).
-define(GAIA_HRL, true).

-include_lib("alsa/include/alsa.hrl").

-define(DEFAULT_PCM_NAME, "plughw:0,0").
-define(DEFAULT_ADDR, {127, 0, 0, 1}).
-define(DEFAULT_ADDR_STRING, "127.0.0.1").
-define(DEFAULT_PORT, 2305).

-define(FORMAT, ?SND_PCM_FORMAT_S16_LE).
-define(CHANNELS, 2).
-define(SAMPLE_SIZE_IN_BYTES, 2).
-define(FRAME_SIZE_IN_BYTES, (?CHANNELS * ?SAMPLE_SIZE_IN_BYTES)).
-define(RATE_IN_HZ, 48000).

-define(PERIOD_SIZE_IN_FRAMES, 960).
-define(PERIOD_SIZE_IN_BYTES, (?PERIOD_SIZE_IN_FRAMES * ?FRAME_SIZE_IN_BYTES)).
-define(PERIOD_SIZE_IN_MS, (?PERIOD_SIZE_IN_FRAMES / (?RATE_IN_HZ / 1000.0))).
-define(BUFFER_MULTIPLICATOR, 8).

-endif.
