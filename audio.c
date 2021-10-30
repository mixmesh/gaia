#include <alloca.h>
#include "audio.h"

// Read https://www.alsa-project.org/wiki/FramesPeriods carefully

int audio_new(char *pcm_name,
              snd_pcm_stream_t stream,
              int mode,
              snd_pcm_format_t format,
              uint8_t channels,
              uint32_t rate_in_hz,
              uint8_t sample_size_in_bytes,
              snd_pcm_uframes_t period_size_in_frames,
              uint8_t buffer_multiplicator,
              audio_info_t **audio_info) {
  int err;
  
  // Open audio device
  snd_pcm_t *pcm;
  if ((err = snd_pcm_open(&pcm, pcm_name, stream, mode)) < 0) {
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

  snd_pcm_uframes_t desired_period_size_in_frames = period_size_in_frames;
  int dir = 0;
  if ((err = snd_pcm_hw_params_set_period_size_near(pcm, hw_params,
                                                    &period_size_in_frames,
                                                    &dir)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }
  if (desired_period_size_in_frames != period_size_in_frames) {
    printf("NOTE: Desired period size was %ld bytes but it was set to %ld\n",
           desired_period_size_in_frames, period_size_in_frames);
  }
  
  snd_pcm_uframes_t desired_buffer_size_in_frames =
    period_size_in_frames * buffer_multiplicator;
  snd_pcm_uframes_t buffer_size_in_frames = desired_buffer_size_in_frames;
  if ((err = snd_pcm_hw_params_set_buffer_size_near(pcm, hw_params,
                                                    &buffer_size_in_frames)) <
      0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }
  if (desired_buffer_size_in_frames != buffer_size_in_frames) {
    printf("NOTE: Desired buffer size was %ld bytes but it was set to %ld\n",
           desired_buffer_size_in_frames, buffer_size_in_frames);
  }

  if ((err = snd_pcm_hw_params(pcm, hw_params)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    return err;
  }

  // Set software parameters for playback stream
  snd_pcm_sw_params_t *sw_params = NULL;

  if (stream == SND_PCM_STREAM_PLAYBACK) {
    if ((err = snd_pcm_sw_params_malloc(&sw_params)) < 0) {
      snd_pcm_hw_params_free(hw_params);
      return err;
    }
    
    if ((err = snd_pcm_sw_params_current(pcm, sw_params)) < 0) {
      snd_pcm_hw_params_free(hw_params);
      snd_pcm_sw_params_free(sw_params);
      return err;
    }
    
    if ((err = snd_pcm_sw_params_set_start_threshold(pcm, sw_params,
                                                     period_size_in_frames)) < 0) {
      snd_pcm_hw_params_free(hw_params);
      snd_pcm_sw_params_free(sw_params);
      return err;
    }
    
    if ((err = snd_pcm_sw_params(pcm, sw_params)) < 0) {
      snd_pcm_hw_params_free(hw_params);
      snd_pcm_sw_params_free(sw_params);
      return err;
    }
  }
  
  // Prepare audio device for use
  if ((err = snd_pcm_prepare(pcm)) < 0) {
    snd_pcm_hw_params_free(hw_params);
    if (sw_params != NULL) {
      snd_pcm_sw_params_free(sw_params);
    }
    return err;
  }
  
  // Instantiate audio_info
  *audio_info = malloc(sizeof(audio_info_t));
  (*audio_info)->pcm = pcm;
  (*audio_info)->hw_params = hw_params;
  (*audio_info)->sw_params = sw_params;
  (*audio_info)->period_size_in_frames = period_size_in_frames;
  
  return 0;
}

void audio_free(audio_info_t *audio_info) {
  snd_pcm_drop(audio_info->pcm);
  snd_pcm_close(audio_info->pcm);
  snd_pcm_hw_params_free(audio_info->hw_params);
  if (audio_info->sw_params != NULL) {
    snd_pcm_sw_params_free(audio_info->sw_params);
  }
  free(audio_info);
}

void audio_print_parameters(audio_info_t *audio_info) {
  snd_output_t *output;
  snd_output_stdio_attach(&output, stderr, 0);
  snd_output_printf(output, "Audio settings:\n");
  snd_pcm_dump_setup(audio_info->pcm, output);
  snd_output_close(output);
}
