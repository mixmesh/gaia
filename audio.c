#include "audio.h"

// Read carefully: https://www.alsa-project.org/wiki/FramesPeriods

int audio_new(char *device_name, snd_pcm_stream_t stream,
              snd_pcm_format_t format, uint8_t channels,
              uint32_t wanted_rate_in_hz, uint8_t sample_size_in_bytes,
              uint32_t latency_in_ms, uint8_t buffer_multiplicator,
              audio_info_t **audio_info) {
  // Open PCM device
  snd_pcm_t *handle;
  int err;
  if ((err = snd_pcm_open(&handle, device_name, stream, 0)) < 0) {
    return err;
  }
  snd_pcm_hw_params_t *params;
  // Set hardware parameters
  if ((err = snd_pcm_hw_params_malloc(&params)) < 0) {
    return err;
  }
  if ((err = snd_pcm_hw_params_any(handle, params)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  if ((err = snd_pcm_hw_params_set_access(handle, params,
                                          SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  if ((err = snd_pcm_hw_params_set_format(handle, params, format)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  if ((err = snd_pcm_hw_params_set_channels(handle, params, channels)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  int dir;
  uint32_t rate_in_hz = wanted_rate_in_hz;
  if ((err = snd_pcm_hw_params_set_rate_near(handle, params, &rate_in_hz,
                                             &dir)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  //assert(rate_in_hz == wanted_rate_in_hz);
  snd_pcm_uframes_t wanted_period_size_in_frames =
    rate_in_hz / (1000 / latency_in_ms);
  snd_pcm_uframes_t period_size_in_frames = wanted_period_size_in_frames;
  if ((err = snd_pcm_hw_params_set_period_size_near(handle, params,
                                                    &period_size_in_frames,
                                                    &dir)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  uint16_t frame_size_in_bytes = channels * sample_size_in_bytes;
  uint32_t period_size_in_bytes = period_size_in_frames * frame_size_in_bytes;
  //assert(period_size_in_frames == wanted_period_size_in_frames);
  snd_pcm_uframes_t wanted_buffer_size_in_frames =
    buffer_multiplicator * period_size_in_frames;
  snd_pcm_uframes_t buffer_size_in_frames = wanted_buffer_size_in_frames;
  if ((err = snd_pcm_hw_params_set_buffer_size_near(handle, params,
                                                    &buffer_size_in_frames)) <
      0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  uint32_t buffer_size_in_bytes = buffer_size_in_frames * frame_size_in_bytes;
  assert(buffer_size_in_frames == wanted_buffer_size_in_frames);
  if ((err = snd_pcm_hw_params(handle, params)) < 0) {
    snd_pcm_hw_params_free(params);
    return err;
  }
  assert(buffer_size_in_frames == wanted_buffer_size_in_frames);
  // Instantiate audio_info
  uint8_t *buffer = malloc(buffer_size_in_bytes);
  *audio_info = malloc(sizeof(audio_info_t));
  (*audio_info)->handle = handle;
  (*audio_info)->params = params;
  (*audio_info)->channels = channels;
  (*audio_info)->sample_size_in_bytes = sample_size_in_bytes;
  (*audio_info)->latency_in_ms = latency_in_ms;
  (*audio_info)->wanted_rate_in_hz = wanted_rate_in_hz;
  (*audio_info)->rate_in_hz = rate_in_hz;
  (*audio_info)->wanted_period_size_in_frames = wanted_period_size_in_frames;
  (*audio_info)->period_size_in_frames = period_size_in_frames;
  (*audio_info)->period_size_in_bytes = period_size_in_bytes;
  (*audio_info)->wanted_buffer_size_in_frames = wanted_buffer_size_in_frames;
  (*audio_info)->buffer_size_in_frames = buffer_size_in_frames;  
  (*audio_info)->buffer_size_in_bytes = buffer_size_in_bytes;
  (*audio_info)->bytes_per_second =
    channels * sample_size_in_bytes * rate_in_hz;
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

void audio_print_info(audio_info_t *audio_info) {
  fprintf(stderr, "latency_in_ms = %d\n\
wanted_rate_in_hz = %d\n\
rate_in_hz = %d\n\
wanted_period_size_in_frames = %ld\n\
period_size_in_frames = %ld\n\
period_size_in_bytes = %d\n\
wanted_buffer_size_in_frames = %ld\n\
buffer_size_in_frames = %ld\n\
buffer_size_in_bytes = %d\n\
bytes_per_second = %d\n",
          audio_info->latency_in_ms,
          audio_info->wanted_rate_in_hz,
          audio_info->rate_in_hz,
          audio_info->wanted_period_size_in_frames,
          audio_info->period_size_in_frames,
          audio_info->period_size_in_bytes,
          audio_info->wanted_buffer_size_in_frames,
          audio_info->buffer_size_in_frames,
          audio_info->buffer_size_in_bytes,
          audio_info->bytes_per_second);
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
