#include <alsa/asoundlib.h>
#include "../audio.h"

int main(int argc, char *argv[])
{
  int i;
  int err;
  int buf[128];
  snd_pcm_t *playback_handle;
  snd_pcm_hw_params_t *hw_params;
  FILE *fin;
  size_t nread;
  unsigned int rate = 44100;	

  if (argc != 3) {
    fprintf(stderr, "Usage: %s card file\n", argv[0]);
    exit(1);
  }

  if ((err = snd_pcm_open (&playback_handle, argv[1], SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
    fprintf (stderr, "cannot open audio device %s (%s)\n", 
	     argv[1],
	     snd_strerror (err));
    exit (1);
  }
		   
  if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0) {
    fprintf (stderr, "cannot allocate hardware parameter structure (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
				 
  if ((err = snd_pcm_hw_params_any (playback_handle, hw_params)) < 0) {
    fprintf (stderr, "cannot initialize hardware parameter structure (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	
  if ((err = snd_pcm_hw_params_set_access (playback_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
    fprintf (stderr, "cannot set access type (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	
  if ((err = snd_pcm_hw_params_set_format (playback_handle, hw_params, SND_PCM_FORMAT_S16_LE)) < 0) {
    fprintf (stderr, "cannot set sample format (%s)\n",
	     snd_strerror (err));
    exit (1);
  }


  if ((err = snd_pcm_hw_params_set_rate_near (playback_handle, hw_params, &rate, 0)) < 0) {
    fprintf (stderr, "cannot set sample rate (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
  printf("Rate set to %d\n", rate);
	
  if ((err = snd_pcm_hw_params_set_channels (playback_handle, hw_params, 2)) < 0) {
    fprintf (stderr, "cannot set channel count (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	
  if ((err = snd_pcm_hw_params (playback_handle, hw_params)) < 0) {
    fprintf (stderr, "cannot set parameters (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	
  snd_pcm_hw_params_free (hw_params);

  /* Display information about the PCM interface */

  unsigned int val, val2;
  
  printf("PCM handle name = '%s'\n",
         snd_pcm_name(playback_handle));

  printf("PCM state = %s\n",
         snd_pcm_state_name(snd_pcm_state(playback_handle)));

  snd_pcm_hw_params_get_access(hw_params,
                          (snd_pcm_access_t *) &val);
  printf("access type = %s\n",
         snd_pcm_access_name((snd_pcm_access_t)val));

  snd_pcm_format_t fval;
  snd_pcm_hw_params_get_format(hw_params, &fval);
  printf("format = '%s' (%s)\n",
    snd_pcm_format_name(fval),
    snd_pcm_format_description(fval));

  snd_pcm_hw_params_get_subformat(hw_params,
                        (snd_pcm_subformat_t *)&val);
  printf("subformat = '%s' (%s)\n",
    snd_pcm_subformat_name((snd_pcm_subformat_t)val),
    snd_pcm_subformat_description(
                          (snd_pcm_subformat_t)val));

  snd_pcm_hw_params_get_channels(hw_params, &val);
  printf("channels = %d\n", val);

  int dir;
  snd_pcm_hw_params_get_rate(hw_params, &val, &dir);
  printf("rate = %d bps\n", val);

  snd_pcm_hw_params_get_period_time(hw_params,
                                    &val, &dir);
  printf("period time = %d us\n", val);

  snd_pcm_uframes_t frames;
  snd_pcm_hw_params_get_period_size(hw_params,
                                    &frames, &dir);
  printf("period size = %d frames\n", (int)frames);

  snd_pcm_hw_params_get_buffer_time(hw_params,
                                    &val, &dir);
  printf("buffer time = %d us\n", val);

  snd_pcm_hw_params_get_buffer_size(hw_params,
                         (snd_pcm_uframes_t *) &val);
  printf("buffer size = %d frames\n", val);

  snd_pcm_hw_params_get_periods(hw_params, &val, &dir);
  printf("periods per buffer = %d frames\n", val);

  snd_pcm_hw_params_get_rate_numden(hw_params,
                                    &val, &val2);
  printf("exact rate = %d/%d bps\n", val, val2);

  val = snd_pcm_hw_params_get_sbits(hw_params);
  printf("significant bits = %d\n", val);

  /*
  snd_pcm_hw_params_get_tick_time(hw_params,
                                  &val, &dir);
  printf("tick time = %d us\n", val);
  */
  
  val = snd_pcm_hw_params_is_batch(hw_params);
  printf("is batch = %d\n", val);

  val = snd_pcm_hw_params_is_block_transfer(hw_params);
  printf("is block transfer = %d\n", val);

  val = snd_pcm_hw_params_is_double(hw_params);
  printf("is double = %d\n", val);

  val = snd_pcm_hw_params_is_half_duplex(hw_params);
  printf("is half duplex = %d\n", val);

  val = snd_pcm_hw_params_is_joint_duplex(hw_params);
  printf("is joint duplex = %d\n", val);

  val = snd_pcm_hw_params_can_overrange(hw_params);
  printf("can overrange = %d\n", val);

  val = snd_pcm_hw_params_can_mmap_sample_resolution(hw_params);
  printf("can mmap = %d\n", val);

  val = snd_pcm_hw_params_can_pause(hw_params);
  printf("can pause = %d\n", val);

  val = snd_pcm_hw_params_can_resume(hw_params);
  printf("can resume = %d\n", val);

  val = snd_pcm_hw_params_can_sync_start(hw_params);
  printf("can sync start = %d\n", val);

  /*
  if ((err = snd_pcm_prepare (playback_handle)) < 0) {
    fprintf (stderr, "cannot prepare audio interface for use (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
  */
	
  if ((fin = fopen(argv[2], "r")) == NULL) {
      fprintf(stderr, "Can't open %s for reading\n", argv[2]);
      exit(1);
  }

  while ((nread = fread(buf, sizeof(int), 128, fin)) > 0) {
    //printf("writing\n");
    if ((err = snd_pcm_writei (playback_handle, buf, nread)) != nread) {
      fprintf (stderr, "write to audio interface failed (%s)\n",
	       snd_strerror (err));
      snd_pcm_prepare(playback_handle);
    }
  }

  snd_pcm_drain(playback_handle);	
  snd_pcm_close (playback_handle);
  exit (0);
}
