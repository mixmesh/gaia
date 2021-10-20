#include "audio.h"

int audio_new(snd_pcm_stream_t stream, audio_info_t **audio_info) {
  snd_pcm_t *handle;
  int err;
  if ((err = snd_pcm_open(&handle, AUDIO_DEVICE, stream, 0)) < 0) {
    return err;
  }
  snd_pcm_hw_params_t *params;
  if ((err = snd_pcm_hw_params_malloc(&params)) < 0) {
    return err;
  }
  if ((err = snd_pcm_hw_params_any(handle, params)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  if ((err = snd_pcm_hw_params_set_access(handle, params, AUDIO_ACCESS)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  if ((err = snd_pcm_hw_params_set_format(handle, params, AUDIO_FORMAT)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  if ((err = snd_pcm_hw_params_set_channels(handle, params,
                                            AUDIO_CHANNELS)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  unsigned int rate = AUDIO_RATE;
  int dir;
  if ((err = snd_pcm_hw_params_set_rate_near(handle, params, &rate,
                                             &dir)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  snd_pcm_uframes_t period_size_in_frames = AUDIO_PERIOD_SIZE_IN_FRAMES;
  if ((err = snd_pcm_hw_params_set_period_size_near(handle, params,
                                                    &period_size_in_frames,
                                                    &dir)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  snd_pcm_uframes_t buffer_size_in_frames = AUDIO_BUFFER_SIZE_IN_FRAMES;
  if ((err = snd_pcm_hw_params_set_buffer_size_near(handle, params,
                                                    &buffer_size_in_frames)) <
      0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  if ((err = snd_pcm_hw_params(handle, params)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  assert(period_size_in_frames == AUDIO_PERIOD_SIZE_IN_FRAMES);
  unsigned int period_size_in_bytes =
    period_size_in_frames * AUDIO_FRAME_SIZE_IN_BYTES;
  buffer_size_in_frames = 2 * period_size_in_frames;
  unsigned int buffer_size_in_bytes =
    buffer_size_in_frames * AUDIO_FRAME_SIZE_IN_BYTES;
  char *buffer = (char *)malloc(buffer_size_in_bytes);
  *audio_info = malloc(sizeof(audio_info_t));
  (*audio_info)->handle = handle;
  (*audio_info)->params = params;
  (*audio_info)->rate = rate;
  (*audio_info)->period_size_in_frames = period_size_in_frames;
  (*audio_info)->buffer_size_in_bytes = buffer_size_in_bytes;
  (*audio_info)->period_size_in_bytes = period_size_in_bytes;
  (*audio_info)->buffer = buffer;
  return 0;
}

void audio_free(audio_info_t *audio_info) {
  snd_pcm_drain(audio_info->handle);
  snd_pcm_close(audio_info->handle);
  snd_pcm_hw_params_free(audio_info->params);
  free(audio_info->buffer);
  free(audio_info);
}

void audio_print_parameters(audio_info_t *audio_info) {
  fprintf(stderr, "\
Hardwired parameters\n\
====================\n\
AUDIO_DEVICE = %s\n\
AUDIO_RATE = %d\n\
AUDIO_SAMPLE_SIZE_IN_BYTES = %d\n\
AUDIO_FRAME_SIZE_IN_BYTES = %d\n\
AUDIO_BYTES_PER_SECOND = %d\nAUDIO_LATENCY_IN_MS = %d\n\
AUDIO_PERIOD_SIZE_IN_FRAMES = %d\n\
AUDIO_PERIOD_SIZE_IN_BYTES = %d\n\
AUDIO_BUFFER_SIZE_IN_FRAMES = %d\n\
AUDIO_BUFFER_SIZE_IN_BYTES = %d\n",
          AUDIO_DEVICE,
          AUDIO_RATE,
          AUDIO_SAMPLE_SIZE_IN_BYTES,
          AUDIO_FRAME_SIZE_IN_BYTES,
          AUDIO_BYTES_PER_SECOND,
          AUDIO_LATENCY_IN_MS,
          AUDIO_PERIOD_SIZE_IN_FRAMES,
          AUDIO_PERIOD_SIZE_IN_BYTES,
          AUDIO_BUFFER_SIZE_IN_FRAMES,
          AUDIO_BUFFER_SIZE_IN_BYTES);

  fprintf(stderr, "Runtime Parameters\n==================\n");
  fprintf(stderr, "pcm_name = '%s'\n", snd_pcm_name(audio_info->handle));

  fprintf(stderr, "pcm_state = '%s'\n",
          snd_pcm_state_name(snd_pcm_state(audio_info->handle)));
  
  snd_pcm_access_t access;
  snd_pcm_hw_params_get_access(audio_info->params, &access);
  fprintf(stderr, "access = %s\n", snd_pcm_access_name(access));
  
  snd_pcm_format_t format;
  snd_pcm_hw_params_get_format(audio_info->params, &format);
  fprintf(stderr, "format = '%s' (%s)\n", snd_pcm_format_name(format),
          snd_pcm_format_description(format));
  
  snd_pcm_subformat_t subformat;
  snd_pcm_hw_params_get_subformat(audio_info->params, &subformat);
  fprintf(stderr, "subformat = '%s' (%s)\n",
          snd_pcm_subformat_name(subformat),
          snd_pcm_subformat_description(subformat));

  unsigned int channels;
  snd_pcm_hw_params_get_channels(audio_info->params, &channels);
  fprintf(stderr, "channels = %d\n", channels);
  
  unsigned int rate;
  int dir;
  snd_pcm_hw_params_get_rate(audio_info->params, &rate, &dir);
  fprintf(stderr, "rate = %d bps\n", rate);

  unsigned int period_time;
  snd_pcm_hw_params_get_period_time(audio_info->params, &period_time, &dir);
  fprintf(stderr, "period_time = %d us\n", period_time);
  
  snd_pcm_uframes_t period_size;
  snd_pcm_hw_params_get_period_size(audio_info->params, &period_size, &dir);
  fprintf(stderr, "period_size = %ld frames\n", period_size);
  
  unsigned int buffer_time;
  snd_pcm_hw_params_get_buffer_time(audio_info->params, &buffer_time, &dir);
  fprintf(stderr, "buffer_time = %d us\n", buffer_time);

  snd_pcm_uframes_t buffer_size;
  snd_pcm_hw_params_get_buffer_size(audio_info->params, &buffer_size);
  fprintf(stderr, "buffer_size = %ld frames\n", buffer_size);

  unsigned int periods;
  snd_pcm_hw_params_get_periods(audio_info->params, &periods, &dir);
  fprintf(stderr, "periods = %d frames\n", periods);
  
  unsigned int rate_num, rate_den;
  snd_pcm_hw_params_get_rate_numden(audio_info->params, &rate_num, &rate_den);
  fprintf(stderr, "rate (exact) = %d/%d bps\n", rate_num, rate_den);

  unsigned int sbits = snd_pcm_hw_params_get_sbits(audio_info->params);
  fprintf(stderr, "sbits = %d\n", sbits);
  
  unsigned int is_batch = snd_pcm_hw_params_is_batch(audio_info->params);
  fprintf(stderr, "is_batch = %d\n", is_batch);

  unsigned int is_block_transfer =
    snd_pcm_hw_params_is_block_transfer(audio_info->params);
  fprintf(stderr, "is_block_transfer = %d\n", is_block_transfer);
  
  unsigned int is_double = snd_pcm_hw_params_is_double(audio_info->params);
  fprintf(stderr, "is_double = %d\n", is_double);
  
  unsigned int is_half_duplex =
    snd_pcm_hw_params_is_half_duplex(audio_info->params);
  fprintf(stderr, "is_half_duplex = %d\n", is_half_duplex);

  unsigned int is_joint_duplex =
    snd_pcm_hw_params_is_joint_duplex(audio_info->params);
  fprintf(stderr, "is_joint_duplex = %d\n", is_joint_duplex);

  unsigned int can_overrange =
    snd_pcm_hw_params_can_overrange(audio_info->params);
  fprintf(stderr, "can_overrange = %d\n", can_overrange);

  unsigned int can_mmap_sample_resolution =
    snd_pcm_hw_params_can_mmap_sample_resolution(audio_info->params);
  fprintf(stderr, "can_mmap_sample_resolution = %d\n",
          can_mmap_sample_resolution);
  
  unsigned int can_pause = snd_pcm_hw_params_can_pause(audio_info->params);
  fprintf(stderr, "can_pause = %d\n", can_pause);
  
  unsigned can_resume = snd_pcm_hw_params_can_resume(audio_info->params);
  fprintf(stderr, "can_resume = %d\n", can_resume);
  
  unsigned int can_sync_start =
    snd_pcm_hw_params_can_sync_start(audio_info->params);
  fprintf(stderr, "can_sync_start = %d\n", can_sync_start);
}
