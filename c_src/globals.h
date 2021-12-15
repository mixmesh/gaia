#ifndef _GLOBALS_H_
#define _GLOBALS_H_

// NOTE: This file *must* be kept in harmony with ../src/globals.hrl

#include <arpa/inet.h>

#define MAX_MEMBERS 128

#define DEFAULT_PCM_NAME "plughw:0,0"
#define DEFAULT_ADDR "127.0.0.1"
#define DEFAULT_PORT 2305

#define FORMAT SND_PCM_FORMAT_S16_LE
#define CHANNELS 1
#define SAMPLE_SIZE_IN_BYTES 2
#define FRAME_SIZE_IN_BYTES (CHANNELS * SAMPLE_SIZE_IN_BYTES)
#define RATE_IN_HZ 16001

#define PERIOD_SIZE_IN_MS 20
#define PERIOD_SIZE_IN_NS (PERIOD_SIZE_IN_MS * 1000000)
#define PERIOD_SIZE_IN_FRAMES                           \
    (uint16_t)(PERIOD_SIZE_IN_MS / 1000.0 * RATE_IN_HZ)
#define PERIOD_SIZE_IN_BYTES (PERIOD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)

#define BUFFER_PERIODS 8
#define start_threshold(period_size_in_frames, buffer_periods)  \
    (period_size_in_frames * (buffer_periods - 1))

#define JITTER_BUFFER_SIZE_IN_MS 400
#define PERIODS_IN_JITTER_BUFFER                                \
    (uint32_t)(JITTER_BUFFER_SIZE_IN_MS / PERIOD_SIZE_IN_MS)
#define JITTER_BUFFER_SIZE_IN_BYTES                             \
    (uint32_t)(PERIODS_IN_JITTER_BUFFER * PERIOD_SIZE_IN_BYTES)
#define JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS (PERIODS_IN_JITTER_BUFFER / 2)

#define OPUS_COMPLEXITY 5
#define OPUS_MAX_PACKET_LEN_IN_BYTES 1276

#define PEAK_AVERAGE_PERIOD_IN_MS 200
#define MAX_MIX_STREAMS 32

// |gaia_id:4|timestamp:8|seqnum:4|packet_len:2|flags:1| = 19 bytes
#define HEADER_SIZE (4 + 8 + 4 + 2 + 1)

#define OPUS_ENABLED_FLAG (1 << 0)

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

#endif
