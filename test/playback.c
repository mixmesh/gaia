#include <alsa/asoundlib.h>
#include "../audio.h"

#define AUDIO_ERROR 3

// $ ./capture > test.dat
// $ aplay --format MU_LAW --file-type raw test.dat
// $ cat test.dat | ./playback

int main() {
  int err;
  
  // Hardwired audio settings
  char *pcm_name = "default";
  snd_pcm_stream_t stream = SND_PCM_STREAM_PLAYBACK;
  int mode = 0;
  snd_pcm_format_t format = SND_PCM_FORMAT_MU_LAW;
  uint8_t channels = 1;
  uint8_t sample_size_in_bytes = 1;
  uint8_t frame_size_in_bytes = channels * sample_size_in_bytes;
  uint32_t rate_in_hz = 8000;
  snd_pcm_uframes_t period_size_in_frames = 128;
  uint32_t period_size_in_bytes = period_size_in_frames * frame_size_in_bytes;
  uint8_t buffer_multiplicator = 3;
  
  // Open audio device
  audio_info_t *audio_info;
  if ((err = audio_new(pcm_name, stream, mode, format, channels, rate_in_hz,
                       sample_size_in_bytes, period_size_in_frames,
                       buffer_multiplicator, &audio_info)) < 0) {
    fprintf(stderr, "Could not initialize audio: %s\n", snd_strerror(err));
    exit(AUDIO_ERROR);
  }
  audio_print_parameters(audio_info);
  
  // Playback 5 seconds of audio data
  unsigned int period_time;
  int dir;
  if ((err = snd_pcm_hw_params_get_period_time(audio_info->hw_params,
                                               &period_time, &dir)) < 0) {
    fprintf(stderr, "Could not get period time: %s\n", snd_strerror(err));
    exit(1);
  }
  long loops = 5000000 / period_time;
  uint8_t *buf = malloc(period_size_in_bytes);

  while (loops-- > 0) {
    ssize_t bytes = read(STDIN_FILENO, buf, period_size_in_bytes);
    if (bytes == 0) {
      break;
    } else if (bytes < 0) {
      perror("Failed to read from stdout");
      break;
    } else if (bytes != period_size_in_bytes) {
      fprintf(stderr,
              "Expected to read %d bytes from stdin but only read %ld\n",
              period_size_in_bytes, bytes);
      break;
    }
    snd_pcm_sframes_t frames =
      snd_pcm_writei(audio_info->pcm, buf, period_size_in_frames);
    if (frames == -EPIPE || frames == -ESTRPIPE) {
      fprintf(stderr, "Failed to write to audio device: %s\n",
              snd_strerror(frames));
      if (snd_pcm_recover(audio_info->pcm, frames, 0) < 0) {
        fprintf(stderr, "Failed to recover audio device: %s\n",
                snd_strerror(frames));
        break;
      }
    } else if (frames < 0) {
      fprintf(stderr, "Failed to write to audio device: %s\n",
              snd_strerror(frames));
      break;
    } else if (frames != period_size_in_frames) {
      fprintf(stderr, "Expected to write %ld frames to audio device but only \
wrote %ld\n",
              period_size_in_frames, frames);
      break;
    }
  }
  
  audio_free(audio_info);
  return 0;
}
