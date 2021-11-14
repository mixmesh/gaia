#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#define PCM_NAME "plughw:0,0"
#define FORMAT SND_PCM_FORMAT_U16_LE
#define CHANNELS 2
#define SAMPLE_SIZE_IN_BYTES 2
#define FRAME_SIZE_IN_BYTES (CHANNELS * SAMPLE_SIZE_IN_BYTES)
#define RATE_IN_HZ 48000

// Let it be a power to 2
#define PERIOD_SIZE_IN_FRAMES 256
#define PERIOD_SIZE_IN_BYTES (PERIOD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)
#define PERIOD_SIZE_IN_MS ((double)PERIOD_SIZE_IN_FRAMES / (RATE_IN_HZ / 1000))
#define BUFFER_MULTIPLICATOR 8

#define PLAYBACK_START_THRESHOLD \
  (PERIOD_SIZE_IN_FRAMES * (BUFFER_MULTIPLICATOR - 1))

#define JITTER_BUFFER_SIZE_IN_MS 200
#define PERIODS_IN_JITTER_BUFFER \
  (uint32_t)(JITTER_BUFFER_SIZE_IN_MS / PERIOD_SIZE_IN_MS)
#define JITTER_BUFFER_SIZE_IN_BYTES \
  (uint32_t)(PERIODS_IN_JITTER_BUFFER * PERIOD_SIZE_IN_BYTES)
#define JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS (PERIODS_IN_JITTER_BUFFER / 2)

#define PAYLOAD_SIZE_IN_FRAMES PERIOD_SIZE_IN_FRAMES
#define PAYLOAD_SIZE_IN_BYTES (PAYLOAD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)

#define PEAK_AVERAGE_PERIOD_IN_MS 200
#define MAX_MIX_STREAMS 4

// |userid:4|timestamp:8|seqnum:4| = 16 bytes
#define HEADER_SIZE (4 + 8 + 4)

#define ARG_ERROR                 1
#define SCHED_ERROR               2
#define THREAD_ERROR              3
#define SOCKET_ERROR              4
#define AUDIO_ERROR               5
#define FILE_ERROR                6
#define AUDIO_SINK_DIED           7
#define FILE_SENDER_DIED          8
#define NETWORK_RECEIVER_DIED     9
#define NETWORK_SENDER_DIED      10
#define INTERNAL_ERROR           11

#define DEFAULT_ADDR "127.0.0.1"
#define DEFAULT_PORT 2305

#endif
