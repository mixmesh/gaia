#ifndef _AUDIO_H_
#define _AUDIO_H_

#include <alsa/asoundlib.h>

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
int audio_read(audio_info_t *audio_info, uint8_t *data,
               snd_pcm_uframes_t nframes);
int audio_non_blocking_write(audio_info_t *audio_info, uint8_t *data,
                             snd_pcm_uframes_t nframes);
int audio_umix16(uint16_t **data, uint8_t n, uint16_t *mixed_data,
                 snd_pcm_uframes_t period_size_in_frames, uint8_t channels);
int audio_smix16(int16_t **data, uint8_t n, int16_t *mixed_data,
                 snd_pcm_uframes_t period_size_in_frames, uint8_t channels);
int audio_dummy_smix16(int16_t **data, uint8_t n, int16_t *mixed_data,
                       snd_pcm_uframes_t period_size_in_frames,
                       uint8_t channels);

#endif
