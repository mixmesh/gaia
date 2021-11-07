#ifndef _AUDIO_H_
#define _AUDIO_H_

#include <alsa/asoundlib.h>

#define AUDIO_NOT_RECOVERED -1000
#define AUDIO_RECOVERED -1001

typedef struct {
  snd_pcm_t *pcm;
  snd_pcm_hw_params_t *hw_params;
  snd_pcm_sw_params_t *sw_params;
  snd_pcm_uframes_t period_size_in_frames;
} audio_info_t;

int audio_new(char *pcm_name, snd_pcm_stream_t stream, int mode,
              snd_pcm_format_t format, uint8_t channels, uint32_t rate_in_hz,
              uint8_t sample_size_in_bytes,
              snd_pcm_uframes_t period_size_in_frames,
              uint8_t buffer_multiplicator, audio_info_t **audio_info);
void audio_free(audio_info_t *audio_info);
void audio_print_parameters(audio_info_t *audio_info, char *who);
snd_pcm_uframes_t audio_write(audio_info_t *audio_info, uint8_t *data,
                              uint32_t nframes);
int audio_read(audio_info_t *audio_info, uint8_t *data, uint32_t nframes);

#endif
