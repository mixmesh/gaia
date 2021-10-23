#include <alloca.h>
#include "audio.h"

// Read https://www.alsa-project.org/wiki/FramesPeriods carefully

int audio_new(char *pcm_name,
              snd_pcm_stream_t stream,
              snd_pcm_format_t format,
              uint8_t channels,
              uint32_t rate_in_hz,
              uint8_t sample_size_in_bytes,
              snd_pcm_uframes_t period_size_in_frames,
              uint8_t buffer_multiplicator,
              audio_info_t **audio_info) {
  // Open PCM
  snd_pcm_t *pcm;
  int err;
  if ((err = snd_pcm_open(&pcm, pcm_name, stream, 0)) < 0) {
    return err;
  }

  // Set hardware parameters
  snd_pcm_hw_params_t *hw_params;
  if ((err = snd_pcm_hw_params_malloc(&hw_params)) < 0) {
    return err;
  }

  if ((err = snd_pcm_hw_params_any(pcm, hw_params)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  if ((err = snd_pcm_hw_params_set_access(pcm, hw_params,
                                          SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  if ((err = snd_pcm_hw_params_set_format(pcm, hw_params, format)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  if ((err = snd_pcm_hw_params_set_channels(pcm, hw_params, channels)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  if ((err = snd_pcm_hw_params_set_rate(pcm, hw_params, rate_in_hz, 0)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  snd_pcm_uframes_t wanted_period_size_in_frames = period_size_in_frames;
  int dir = 0;
  if ((err = snd_pcm_hw_params_set_period_size_near(pcm, hw_params,
                                                    &wanted_period_size_in_frames,
                                                    &dir)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }
  assert(wanted_period_size_in_frames == period_size_in_frames);

  snd_pcm_uframes_t buffer_size_in_frames =
    period_size_in_frames * buffer_multiplicator;
  if ((err = snd_pcm_hw_params_set_buffer_size_near(pcm, hw_params,
                                                    &buffer_size_in_frames)) <
      0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  if ((err = snd_pcm_hw_params(pcm, hw_params)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  snd_pcm_prepare(pcm);

  // Instantiate audio_info
  *audio_info = malloc(sizeof(audio_info_t));
  (*audio_info)->pcm = pcm;
  (*audio_info)->hw_params = hw_params;

  return 0;
}

void audio_free(audio_info_t *audio_info) {
  snd_pcm_drain(audio_info->pcm);
  snd_pcm_close(audio_info->pcm);
  snd_pcm_hw_params_free(audio_info->hw_params);
  free(audio_info);
}

void audio_print_parameters(audio_info_t *audio_info) {
  snd_output_t *output;
  snd_output_stdio_attach(&output, stderr, 0);
  snd_output_printf(output, "Audio settings:\n");
  snd_pcm_dump_setup(audio_info->pcm, output);
  snd_output_close(output);
}
