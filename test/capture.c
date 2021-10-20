#include <alsa/asoundlib.h>
#include "../audio.h"

int main() {
  // Open ALSA device
  audio_info_t *audio_info;
  int err;
  if ((err = audio_new(SND_PCM_STREAM_CAPTURE, &audio_info)) < 0) {
    fprintf(stderr, "could not initialize audio: %s\n", snd_strerror(err));
    exit(1);
  }
  audio_print_parameters(audio_info);
  // Capture 5 seconds of audio data
  unsigned int period_time;
  int dir;
  if ((err = snd_pcm_hw_params_get_period_time(audio_info->params,
                                               &period_time, &dir)) < 0) {
    fprintf(stderr, "could not get period time: %s\n", snd_strerror(err));
    exit(1);
  }
  long loops = 5000000 / period_time;
  while (loops > 0) {
    loops--;
    snd_pcm_sframes_t frames =
      snd_pcm_readi(audio_info->handle, audio_info->buffer,
                    audio_info->period_size_in_frames);
    if (frames == -EPIPE) {
      fprintf(stderr, "overrun occurred\n");
      snd_pcm_prepare(audio_info->handle);
    } else if (frames < 0) {
      fprintf(stderr, "error from read: %s\n", snd_strerror(frames));
    } else if (frames != audio_info->period_size_in_frames) {
      fprintf(stderr, "short read, read %ld frames\n", frames);
    }
    ssize_t n = write(1, audio_info->buffer, audio_info->period_size_in_bytes);
    if (n != audio_info->period_size_in_bytes) {
      fprintf(stderr, "short write: wrote %ld bytes\n", n);
    }
  }
  audio_free(audio_info);
  return 0;
}
