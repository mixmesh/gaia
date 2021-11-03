#ifndef _GLOBALS_H_
#define _GLOBALS_H_

#define PCM_NAME "plughw:0,0"
#define FORMAT SND_PCM_FORMAT_S16_LE
#define CHANNELS 2
#define SAMPLE_SIZE_IN_BYTES 2
#define FRAME_SIZE_IN_BYTES (CHANNELS * SAMPLE_SIZE_IN_BYTES)
#define RATE_IN_HZ 48000

#define SENDER_PERIOD_SIZE_IN_FRAMES 512
#define SENDER_PERIOD_SIZE_IN_BYTES \
  (SENDER_PERIOD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)
#define SENDER_MODE 0
#define SENDER_BUFFER_MULTIPLICATOR 4

#define RECEIVER_PERIOD_SIZE_IN_FRAMES (SENDER_PERIOD_SIZE_IN_FRAMES * 4)
#define RECEIVER_PERIOD_SIZE_IN_BYTES \
  (RECEIVER_PERIOD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)
#define RECEIVER_MODE SND_PCM_NONBLOCK
#define RECEIVER_BUFFER_MULTIPLICATOR 10

#define PAYLOAD_SIZE_IN_FRAMES SENDER_PERIOD_SIZE_IN_FRAMES
#define PAYLOAD_SIZE_IN_BYTES (PAYLOAD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)

// |userid:4|timestamp:8|seqnum:4| = 16 bytes
#define HEADER_SIZE (4 + 8 + 4)

#define SOCKET_ERROR -102
#define AUDIO_ERROR -103

#endif
