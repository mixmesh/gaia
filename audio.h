#ifndef _AUDIO_H
#define _AUDIO_H_

#include <alsa/asoundlib.h>

typedef struct {
  snd_pcm_t *handle;
  snd_pcm_hw_params_t *params;
  uint8_t channels;
  uint8_t sample_size_in_bytes;
  uint32_t latency_in_ms;
  uint32_t wanted_rate_in_hz;
  uint32_t rate_in_hz;
  snd_pcm_uframes_t wanted_period_size_in_frames;
  snd_pcm_uframes_t period_size_in_frames;
  uint32_t period_size_in_bytes;
  snd_pcm_uframes_t wanted_buffer_size_in_frames;
  snd_pcm_uframes_t buffer_size_in_frames;
  uint32_t buffer_size_in_bytes;  
  uint32_t bytes_per_second;
  uint8_t *buffer;
} audio_info_t;

int audio_new(char *device_name, snd_pcm_stream_t stream,
              snd_pcm_format_t format, uint8_t channels,
              uint32_t wanted_rate_in_hz, uint8_t sample_size_in_bytes,
              uint32_t latency_in_ms, uint8_t buffer_multiplicator,
              audio_info_t **audio_info);
void audio_free(audio_info_t *audio_info);
void audio_print_info(audio_info_t *audio_info);

#endif
