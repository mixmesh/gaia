#ifndef _AUDIO_H
#define _AUDIO_H_

#include <alsa/asoundlib.h>

// https://www.alsa-project.org/wiki/FramesPeriods

#define USE_MU_LAW
#ifdef USE_MU_LAW
#define AUDIO_FORMAT SND_PCM_FORMAT_MU_LAW
#define AUDIO_CHANNELS 1
#define AUDIO_RATE 8000
#define AUDIO_SAMPLE_SIZE_IN_BYTES 1
#endif

#define AUDIO_DEVICE "default"
#define AUDIO_LATENCY_IN_MS 50

#define AUDIO_ACCESS SND_PCM_ACCESS_RW_INTERLEAVED
#define AUDIO_FRAME_SIZE_IN_BYTES (AUDIO_CHANNELS * AUDIO_SAMPLE_SIZE_IN_BYTES)
#define AUDIO_BYTES_PER_SECOND (AUDIO_CHANNELS * AUDIO_SAMPLE_SIZE_IN_BYTES * AUDIO_RATE)
#define AUDIO_PERIOD_SIZE_IN_FRAMES (AUDIO_RATE / (1000 / AUDIO_LATENCY_IN_MS))
#define AUDIO_PERIOD_SIZE_IN_BYTES (AUDIO_PERIOD_SIZE_IN_FRAMES * AUDIO_FRAME_SIZE_IN_BYTES)
#define AUDIO_BUFFER_SIZE_IN_FRAMES (3 * AUDIO_PERIOD_SIZE_IN_FRAMES)
#define AUDIO_BUFFER_SIZE_IN_BYTES (AUDIO_BUFFER_SIZE_IN_FRAMES * AUDIO_FRAME_SIZE_IN_BYTES)

typedef struct {
  snd_pcm_t *handle;
  snd_pcm_hw_params_t *params;
  unsigned int rate;
  snd_pcm_uframes_t period_size_in_frames;
  unsigned int buffer_size_in_bytes;  
  unsigned int period_size_in_bytes;
  char *buffer;
} audio_info_t;

int audio_new(audio_info_t **audio_info);
void audio_free(audio_info_t *audio_info);
void audio_print_parameters(audio_info_t *audio_info);
  
#endif
