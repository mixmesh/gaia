#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#define PCM_NAME "plughw:0,0"
#define FORMAT SND_PCM_FORMAT_U16_LE
#define CHANNELS 2
#define SAMPLE_SIZE_IN_BYTES 2
#define FRAME_SIZE_IN_BYTES (CHANNELS * SAMPLE_SIZE_IN_BYTES)
#define RATE_IN_HZ 48000

#define PERIOD_SIZE_IN_FRAMES 256
#define PERIOD_SIZE_IN_BYTES (PERIOD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)
#define PERIOD_SIZE_IN_MS ((double)PERIOD_SIZE_IN_FRAMES / (RATE_IN_HZ / 1000))
#define BUFFER_MULTIPLICATOR 4

#define JITTER_BUFFER_SIZE_IN_MS 100
#define PERIODS_IN_JITTER_BUFFER \
  (uint32_t)(JITTER_BUFFER_SIZE_IN_MS / PERIOD_SIZE_IN_MS)
#define JITTER_BUFFER_SIZE_IN_BYTES \
  (uint32_t)(PERIODS_IN_JITTER_BUFFER * PERIOD_SIZE_IN_BYTES)
#define JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS (PERIODS_IN_JITTER_BUFFER / 2)

#define PAYLOAD_SIZE_IN_FRAMES PERIOD_SIZE_IN_FRAMES
#define PAYLOAD_SIZE_IN_BYTES (PAYLOAD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)

// |userid:4|timestamp:8|seqnum:4| = 16 bytes
#define HEADER_SIZE (4 + 8 + 4)

#define SOCKET_ERROR -102
#define AUDIO_ERROR -103

#endif
