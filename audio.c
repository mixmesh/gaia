#include "audio.h"
#include "globals.h"

// Read https://www.alsa-project.org/wiki/FramesPeriods carefully

int audio_new(char *pcm_name, snd_pcm_stream_t stream, int mode,
              snd_pcm_format_t format, uint8_t channels, uint32_t rate_in_hz,
              uint8_t sample_size_in_bytes,
              snd_pcm_uframes_t period_size_in_frames,
              uint8_t buffer_multiplicator, audio_info_t **audio_info) {
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
    fprintf(stderr,
            "NOTE: Desired period size was %ld bytes but it was set to %ld\n",
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
    fprintf(stderr,
            "NOTE: Desired buffer size was %ld bytes but it was set to %ld\n",
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
    
    if ((err =
         snd_pcm_sw_params_set_start_threshold(pcm, sw_params,
                                               PLAYBACK_START_THRESHOLD)) < 0) {
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
  snd_pcm_drain(audio_info->pcm);
  snd_pcm_close(audio_info->pcm);
  snd_pcm_hw_params_free(audio_info->hw_params);
  if (audio_info->sw_params != NULL) {
    snd_pcm_sw_params_free(audio_info->sw_params);
  }
  free(audio_info);
}

void audio_print_parameters(audio_info_t *audio_info, char *who) {
  snd_output_t *output;
  snd_output_stdio_attach(&output, stderr, 0);
  snd_output_printf(output, "Audio %s settings:\n", who);
  snd_pcm_dump_setup(audio_info->pcm, output);
  snd_output_close(output);
}

snd_pcm_uframes_t audio_write(audio_info_t *audio_info, uint8_t *data,
                              snd_pcm_uframes_t nframes) {
  snd_pcm_uframes_t written_frames = 0;
  while (written_frames < nframes) {
    snd_pcm_uframes_t frames =
      snd_pcm_writei(audio_info->pcm, &data[written_frames],
                     nframes - written_frames);
    if (frames == -EPIPE || frames == -ESTRPIPE) {
      printf("snd_pcm_writei: Failed to write to audio device: %s\n",
             snd_strerror(frames));
      if (snd_pcm_recover(audio_info->pcm, frames, 0) < 0) {
        fprintf(stderr, "snd_pcm_readi: Failed to recover audio device: %s\n",
                snd_strerror(frames));
        return AUDIO_NOT_RECOVERED;
      }
      frames = 0;
    } else if (frames < 0) {
      fprintf(stderr, "snd_pcm_writei: Failed to write to audio device: %s\n",
              snd_strerror(frames));
      return frames;
    }
    written_frames += frames;
    if (written_frames < nframes) {
      printf("snd_pcm_writei: %ld frames to go...\n", nframes - written_frames);
    }
  }
  return written_frames;
}

int audio_read(audio_info_t *audio_info, uint8_t *data,
               snd_pcm_uframes_t nframes) {
  snd_pcm_uframes_t read_frames = 0;
  while (read_frames < nframes) {
    snd_pcm_uframes_t frames =
      snd_pcm_readi(audio_info->pcm, &data[read_frames], nframes - read_frames);
    if (frames == -EPIPE || frames == -ESTRPIPE) {
      printf("snd_pcm_readi: Failed to read from audio device: %s\n",
             snd_strerror(frames));
      if (snd_pcm_recover(audio_info->pcm, frames, 0) < 0) {
        fprintf(stderr, "snd_pcm_readi: Failed to recover audio device: %s\n",
                snd_strerror(frames));
        return AUDIO_NOT_RECOVERED;
      }
      frames = 0;
    } else if (frames < 0) {
      fprintf(stderr, "snd_pcm_readi: Failed to read from audio device: %s\n",
              snd_strerror(frames));
      return frames;
    }
    read_frames += frames;
    if (read_frames < nframes) {
      printf("snd_pcm_readi: %ld frames to go...\n", nframes - read_frames);
    }
  }
  return read_frames;
}

/*
int audio_mix(
http://www.vttoth.com/CMS/index.php/technical-notes/68
https://stackoverflow.com/a/25102339




int a = 111; // first sample (-32768..32767)
int b = 222; // second sample
int m; // mixed result will go here

// Make both samples unsigned (0..65535)
a += 32768;
b += 32768;

// Pick the equation
if ((a < 32768) || (b < 32768)) {
    // Viktor's first equation when both sources are "quiet"
    // (i.e. less than middle of the dynamic range)
    m = a * b / 32768;
} else {
    // Viktor's second equation when one or both sources are loud
    m = 2 * (a + b) - (a * b) / 32768 - 65536;
}

// Output is unsigned (0..65536) so convert back to signed (-32768..32767)
if (m == 65536) m = 65535;
m -= 32768;
*/
