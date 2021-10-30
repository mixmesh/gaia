#ifndef _AUDIO_H
#define _AUDIO_H_

#include <alsa/asoundlib.h>

typedef struct {
  snd_pcm_t *pcm;
  snd_pcm_hw_params_t *hw_params;
  snd_pcm_sw_params_t *sw_params;
  snd_pcm_uframes_t period_size_in_frames;
} audio_info_t;

int audio_new(char *pcm_name,
              snd_pcm_stream_t stream,
              int mode,
              snd_pcm_format_t format,
              uint8_t channels,
              uint32_t rate_in_hz,
              uint8_t sample_size_in_bytes,
              snd_pcm_uframes_t period_size_in_frames,
              uint8_t buffer_multiplicator,
              audio_info_t **audio_info);
void audio_free(audio_info_t *audio_info);
void audio_print_parameters(audio_info_t *audio_info);

#endif
