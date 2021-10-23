#include <stdbool.h>
#include <signal.h>
#include <sched.h>
#include <arpa/inet.h>
#include "audio.h"
#include "timing.h"

#define DEFAULT_HOST "127.0.0.1"
#define DEFAULT_PORT 5422

// |userid:4|seqnum:4|timestamp:4| = 12 bytes
#define HEADER_SIZE (4 + 4 + 4)

#define SCHED_ERROR 1
#define SOCKET_ERROR 2
#define AUDIO_ERROR 3
#define ARG_ERROR 4

audio_info_t *audio_info = NULL;
int sockfd = -1;
uint8_t *udp_buf = NULL;

void usage(char *command, int status) {
  fprintf(stderr, "Usage: %s userid [host] [port]\n", command);
  fprintf(stderr, "Example: sudo ./client 1 172.16.0.116 2305\n");
  exit(status);
}

void sigint(int sig) {
  if (audio_info != NULL) {
    audio_free(audio_info);
  }
  if (sockfd != -1) {
    close(sockfd);
  }
  if (udp_buf != NULL) {
    free(udp_buf);
  }
  exit(0);
}

void start_sending(uint32_t userid, in_addr_t host, uint16_t port) {
  // Hardwired audio settings
  char *pcm_name = "hw:0,0";
  snd_pcm_stream_t stream = SND_PCM_STREAM_CAPTURE;
  snd_pcm_format_t format = SND_PCM_FORMAT_S16_LE;
  uint8_t channels = 2;
  uint8_t sample_size_in_bytes = 2;
  uint32_t rate_in_hz = 48000;
  snd_pcm_uframes_t period_size_in_frames = 128;
  uint8_t buffer_multiplicator = 4;

  // Handle Ctrl-c
  signal(SIGINT, sigint);

  // Set scheduling parameters
  struct sched_param sched_param;
  if (sched_getparam(0, &sched_param) < 0) {
    perror("Could not get scheduling parameters");
    exit(SCHED_ERROR);
  }
  sched_param.sched_priority = sched_get_priority_max(SCHED_FIFO);
  if (sched_setscheduler(0, SCHED_FIFO, &sched_param) == -1) {
    perror("Could not set FIFO scheduling policy");
    exit(SCHED_ERROR);
  }
  fprintf(stderr, "FIFO scheduling policy has been set with priority %i\n",
          sched_param.sched_priority);

  // Create socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("Socket creation failed");
    exit(SOCKET_ERROR);
  }

  struct sockaddr_in dest_addr = {0};
  dest_addr.sin_family = AF_INET;
  dest_addr.sin_port = htons(port);
  dest_addr.sin_addr.s_addr = host;

  // Open audio device
  int err;
  if ((err = audio_new(pcm_name, stream, format, channels, rate_in_hz,
                       sample_size_in_bytes, period_size_in_frames,
                       buffer_multiplicator, &audio_info)) < 0) {
    fprintf(stderr, "Could not initialize audio: %s\n", snd_strerror(err));
    exit(AUDIO_ERROR);
  }
  audio_print_parameters(audio_info);

  uint32_t period_size_in_bytes =
    period_size_in_frames * channels * sample_size_in_bytes;
  double period_size_in_ms =
    (double)period_size_in_frames / (rate_in_hz / 1000);
  fprintf(stderr, "Period size is %d bytes (%fms)\n", period_size_in_bytes,
          period_size_in_ms);
  uint32_t udp_buf_size = HEADER_SIZE + period_size_in_bytes;
  udp_buf = malloc(udp_buf_size);

  uint32_t seqnum = 0;

  // Add userid to header
  udp_buf[0] = userid & 0xff;
  udp_buf[1] = (userid >> 8) & 0xff;
  udp_buf[2] = (userid >> 16) & 0xff;
  udp_buf[3] = (userid >> 24) & 0xff;

  while (true) {
    // Add sequence number to header
    udp_buf[4] = seqnum & 0xff;
    udp_buf[5] = (seqnum >> 8) & 0xff;
    udp_buf[6] = (seqnum >> 16) & 0xff;
    udp_buf[7] = (seqnum >> 24) & 0xff;

    // Add timestamp to header
    uint32_t timestamp = get_timestamp();
    udp_buf[8] = timestamp & 0xff;
    udp_buf[9] = (timestamp >> 8) & 0xff;
    udp_buf[10] = (timestamp >> 16) & 0xff;
    udp_buf[11] = (timestamp >> 24) & 0xff;

    // Read audio data
    snd_pcm_uframes_t frames =
      snd_pcm_readi(audio_info->pcm, &udp_buf[HEADER_SIZE],
                    period_size_in_frames);
    if (frames == -EBADFD) {
      fprintf(stderr, "Bad pcm state\n");
    } else if (frames == -EPIPE) {
      fprintf(stderr, "Overrun occurred\n");
      assert(snd_pcm_recover(audio_info->pcm, frames, 0) == 0);
    } else if (frames == -ESTRPIPE) {
      fprintf(stderr, "Suspend event occurred \n");
      assert(snd_pcm_recover(audio_info->pcm, frames, 0) == 0);
    } else if (frames != period_size_in_frames) {
      fprintf(stderr, "Short read, read %lu frames\n", frames);
    }

    // Send audio data:
    // |userid:4|seqnum:4|timestamp:4|data:period_size_in_bytes| = udp_buf_size
    ssize_t sent_bytes;
    if ((sent_bytes = sendto(sockfd, &udp_buf[HEADER_SIZE],
                             period_size_in_bytes, 0,
                             (struct sockaddr *)&dest_addr,
                             sizeof(dest_addr))) < 0) {
      perror("All bytes could not be sent");
    }

    seqnum++;
  }
}

int main (int argc, char *argv[]) {
  if (argc < 2 || argc > 4) {
    usage(argv[0], 1);
  }

  // Read userid
  char *endptr;
  int32_t userid = strtol(argv[1], &endptr, 10);
  if (strlen(endptr) != 0 || userid < 1) {
    usage(argv[0], ARG_ERROR);
  }
  
  // Read host
  in_addr_t host = inet_addr(DEFAULT_HOST);
  if (argc > 2) {
    if ((host = inet_addr(argv[2])) == -1) {
      usage(argv[0], ARG_ERROR);
    }
  }
  
  // Read port
  int32_t port = DEFAULT_PORT;
  if (argc > 3) {
    port = strtol(argv[3], &endptr, 10);
    if (strlen(endptr) != 0 || port < 1) {
      usage(argv[0], ARG_ERROR);
    }
  }
  
  start_sending(userid, host, port);
  
  return 0;
}
