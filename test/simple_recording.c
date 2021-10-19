#include <alsa/asoundlib.h>

// https://www.alsa-project.org/wiki/FramesPeriods
#define ACCESS SND_PCM_ACCESS_RW_INTERLEAVED
#define FORMAT SND_PCM_FORMAT_MU_LAW
#define CHANNELS 1

#define DEVICE "default"
#define RATE 8000
#define SAMPLE_SIZE_IN_BYTES 1
#define FRAME_SIZE_IN_BYTES (CHANNELS * SAMPLE_SIZE_IN_BYTES)
#define BYTES_PER_SECOND (CHANNELS * SAMPLE_SIZE_IN_BYTES * RATE)
#define LATENCY_IN_MS 50
#define PERIOD_SIZE_IN_FRAMES (RATE / (1000 / LATENCY_IN_MS))
#define PERIOD_SIZE_IN_BYTES (PERIOD_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)
#define BUFFER_SIZE_IN_FRAMES (3 * PERIOD_SIZE_IN_FRAMES)
#define BUFFER_SIZE_IN_BYTES (BUFFER_SIZE_IN_FRAMES * FRAME_SIZE_IN_BYTES)

int main() {
  fprintf(stderr, "DEVICE = %s\nRATE = %d\nSAMPLE_SIZE_IN_BYTES = %d\nFRAME_SIZE_IN_BYTES = %d\nBYTES_PER_SECOND = %d\nLATENCY_IN_MS = %d\nPERIOD_SIZE_IN_FRAMES = %d\nPERIOD_SIZE_IN_BYTES = %d\nBUFFER_SIZE_IN_FRAMES = %d\nBUFFER_SIZE_IN_BYTES = %d\n", DEVICE, RATE, SAMPLE_SIZE_IN_BYTES, FRAME_SIZE_IN_BYTES, BYTES_PER_SECOND, LATENCY_IN_MS, PERIOD_SIZE_IN_FRAMES, PERIOD_SIZE_IN_BYTES, BUFFER_SIZE_IN_FRAMES, BUFFER_SIZE_IN_BYTES);

  snd_pcm_t *handle;
  int result = snd_pcm_open(&handle, DEVICE, SND_PCM_STREAM_CAPTURE, 0);
  if (result < 0) {
    fprintf(stderr, "unable to open pcm device: %s\n", snd_strerror(result));
    exit(1);
  }
  
  snd_pcm_hw_params_t *params;
  snd_pcm_hw_params_alloca(&params);  

  snd_pcm_hw_params_any(handle, params);

  snd_pcm_hw_params_set_access(handle, params, ACCESS);
  snd_pcm_hw_params_set_format(handle, params, FORMAT);
  snd_pcm_hw_params_set_channels(handle, params, CHANNELS);
  unsigned int val = RATE;
  int dir;
  snd_pcm_hw_params_set_rate_near(handle, params, &val, &dir);
  snd_pcm_uframes_t period_size_in_frames = PERIOD_SIZE_IN_FRAMES;
  snd_pcm_hw_params_set_period_size_near(handle, params,
                                         &period_size_in_frames, &dir);
  snd_pcm_uframes_t buffer_size_in_frames = BUFFER_SIZE_IN_FRAMES;
  snd_pcm_hw_params_set_buffer_size_near(handle, params,
                                         &buffer_size_in_frames);
  
  result = snd_pcm_hw_params(handle, params);
  if (result < 0) {
    fprintf(stderr, "unable to set hw parameters: %s\n", snd_strerror(result));
    exit(1);
  }
  
  snd_pcm_hw_params_get_period_size(params, &period_size_in_frames, &dir);
  buffer_size_in_frames = 2 * period_size_in_frames;
  int buffer_size_in_bytes = buffer_size_in_frames * FRAME_SIZE_IN_BYTES;
  char *buf = (char *)malloc(buffer_size_in_bytes);

  /* Display information about the PCM interface */
  unsigned int val2;
  fprintf(stderr, "PCM handle name = '%s'\n", snd_pcm_name(handle));

  fprintf(stderr, "PCM state = %s\n",
          snd_pcm_state_name(snd_pcm_state(handle)));

  snd_pcm_hw_params_get_access(params, (snd_pcm_access_t *) &val);
  fprintf(stderr, "access type = %s\n",
          snd_pcm_access_name((snd_pcm_access_t)val));
  
  snd_pcm_format_t fval;
  snd_pcm_hw_params_get_format(params, &fval);
  fprintf(stderr, "format = '%s' (%s)\n",
          snd_pcm_format_name(fval),
          snd_pcm_format_description(fval));
  
  snd_pcm_hw_params_get_subformat(params, (snd_pcm_subformat_t *)&val);
  fprintf(stderr, "subformat = '%s' (%s)\n",
          snd_pcm_subformat_name((snd_pcm_subformat_t)val),
          snd_pcm_subformat_description((snd_pcm_subformat_t)val));
  
  snd_pcm_hw_params_get_channels(params, &val);
  fprintf(stderr, "channels = %d\n", val);
  
  snd_pcm_hw_params_get_rate(params, &val, &dir);
  fprintf(stderr, "rate = %d bps\n", val);
  
  snd_pcm_hw_params_get_period_time(params, &val, &dir);
  fprintf(stderr, "period time = %d us\n", val);
  
  snd_pcm_hw_params_get_period_size(params, &period_size_in_frames, &dir);
  fprintf(stderr, "period size = %d frames\n", (int)period_size_in_frames);
  
  snd_pcm_hw_params_get_buffer_time(params, &val, &dir);
  fprintf(stderr, "buffer time = %d us\n", val);

  snd_pcm_hw_params_get_buffer_size(params, (snd_pcm_uframes_t *) &val);
  fprintf(stderr, "buffer size = %d frames\n", val);
  
  snd_pcm_hw_params_get_periods(params, &val, &dir);
  fprintf(stderr, "periods per buffer = %d frames\n", val);
  
  snd_pcm_hw_params_get_rate_numden(params, &val, &val2);
  fprintf(stderr, "exact rate = %d/%d bps\n", val, val2);
  
  val = snd_pcm_hw_params_get_sbits(params);
  fprintf(stderr, "significant bits = %d\n", val);
  
  /*
  snd_pcm_hw_params_get_tick_time(params, &val, &dir);
  fprintf(stderr, "tick time = %d us\n", val);
  */
  
  val = snd_pcm_hw_params_is_batch(params);
  fprintf(stderr, "is batch = %d\n", val);
  
  val = snd_pcm_hw_params_is_block_transfer(params);
  fprintf(stderr, "is block transfer = %d\n", val);
  
  val = snd_pcm_hw_params_is_double(params);
  fprintf(stderr, "is double = %d\n", val);
  
  val = snd_pcm_hw_params_is_half_duplex(params);
  fprintf(stderr, "is half duplex = %d\n", val);
  
  val = snd_pcm_hw_params_is_joint_duplex(params);
  fprintf(stderr, "is joint duplex = %d\n", val);
  
  val = snd_pcm_hw_params_can_overrange(params);
  fprintf(stderr, "can overrange = %d\n", val);
  
  val = snd_pcm_hw_params_can_mmap_sample_resolution(params);
  fprintf(stderr, "can mmap = %d\n", val);
  
  val = snd_pcm_hw_params_can_pause(params);
  fprintf(stderr, "can pause = %d\n", val);
  
  val = snd_pcm_hw_params_can_resume(params);
  fprintf(stderr, "can resume = %d\n", val);
  
  val = snd_pcm_hw_params_can_sync_start(params);
  fprintf(stderr, "can sync start = %d\n", val);
  
  /* We want to loop for 5 seconds */
  snd_pcm_hw_params_get_period_time(params, &val, &dir);
  long loops = 5000000 / val;

  while (loops > 0) {
    loops--;
    result = snd_pcm_readi(handle, buf, period_size_in_frames);
    if (result == -EPIPE) {
      /* EPIPE means overrun */
      fprintf(stderr, "overrun occurred\n");
      snd_pcm_prepare(handle);
    } else if (result < 0) {
      fprintf(stderr, "error from read: %s\n", snd_strerror(result));
    } else if (result != (int)period_size_in_frames) {
      fprintf(stderr, "short read, read %d frames\n", result);
    }
    result = write(1, buf, PERIOD_SIZE_IN_BYTES);
    if (result != PERIOD_SIZE_IN_BYTES) {
      fprintf(stderr, "short write: wrote %d bytes\n", result);
    }
  }
  
  snd_pcm_drain(handle);
  snd_pcm_close(handle);
  free(buf);
  
  return 0;
}
