/**
 * alsa_capture.c
 */

/**
 * Paul Davis
 * http://equalarea.com/paul/alsa-audio.html#howto
 */

/**
 * Jan Newmarch
 */

#include <stdio.h>
#include <stdlib.h>
#include <alsa/asoundlib.h>
#include <signal.h>

#define BUFSIZE 128
#define RATE 44100

FILE *fout = NULL;

/*
 * quit on ctrl-c
 */
void sigint(int sig) {
  if (fout != NULL) {
    fclose(fout);
  }
  exit(1);
}
	      
int main (int argc, char *argv[])
{
  int i;
  int err;
  short buf[BUFSIZE];
  snd_pcm_t *capture_handle;
  snd_pcm_hw_params_t *hw_params;
  unsigned int rate = RATE;	
  int nread;

  if (argc != 3) {
    fprintf(stderr, "Usage: %s cardname file\n", argv[0]);
    exit(1);
  }

  if ((fout = fopen(argv[2], "w")) == NULL) {
    fprintf(stderr, "Can't open %s for writing\n", argv[2]);
    exit(1);
  }


  signal(SIGINT, sigint);
	
  if ((err = snd_pcm_open (&capture_handle, argv[1], SND_PCM_STREAM_CAPTURE, 0)) < 0) {
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
				 
  if ((err = snd_pcm_hw_params_any (capture_handle, hw_params)) < 0) {
    fprintf (stderr, "cannot initialize hardware parameter structure (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	
  if ((err = snd_pcm_hw_params_set_access (capture_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
    fprintf (stderr, "cannot set access type (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	

  if ((err = snd_pcm_hw_params_set_format (capture_handle, hw_params, SND_PCM_FORMAT_S16_LE)) < 0) {
    fprintf (stderr, "cannot set sample format (%s)\n",
	     snd_strerror (err));
    exit (1);
  }

  if ((err = snd_pcm_hw_params_set_rate_near (capture_handle, hw_params, &rate, 0)) < 0) {
    fprintf (stderr, "cannot set sample rate (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
  fprintf(stderr, "rate set to %d\n", rate);
	
  if ((err = snd_pcm_hw_params_set_channels (capture_handle, hw_params, 2)) < 0) {
    fprintf (stderr, "cannot set channel count (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	
  if ((err = snd_pcm_hw_params (capture_handle, hw_params)) < 0) {
    fprintf (stderr, "cannot set parameters (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
	
  snd_pcm_hw_params_free (hw_params);

  /* Display information about the PCM interface */

  unsigned int val, val2;
  
  printf("PCM handle name = '%s'\n",
         snd_pcm_name(capture_handle));

  printf("PCM state = %s\n",
         snd_pcm_state_name(snd_pcm_state(capture_handle)));

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
  if ((err = snd_pcm_prepare (capture_handle)) < 0) {
    fprintf (stderr, "cannot prepare audio interface for use (%s)\n",
	     snd_strerror (err));
    exit (1);
  }
  */
	
  while (1) {
    if ((nread = snd_pcm_readi (capture_handle, buf, BUFSIZE)) < 0) {
      fprintf (stderr, "read from audio interface failed (%s)\n",
	       snd_strerror (err));
      /* recover */
      snd_pcm_prepare(capture_handle);
    } else {
      fwrite(buf, sizeof(short), nread, fout);
    }
  }
	
  snd_pcm_close (capture_handle);
  exit(0);
}
