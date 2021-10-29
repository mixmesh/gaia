#include <stdbool.h>
#include <signal.h>
#include <arpa/inet.h>
#include "audio.h"
#include "timing.h"
#include "scheduling.h"

#define DEFAULT_HOST "127.0.0.1"
#define DEFAULT_PORT 5422

// |userid:4|seqnum:4|timestamp:8| = 16 bytes
#define HEADER_SIZE (4 + 4 + 8)

#define SCHED_ERROR 1
#define SOCKET_ERROR 2
#define AUDIO_ERROR 3
#define ARG_ERROR 4

audio_info_t *audio_info = NULL;
int sockfd = -1;
uint8_t *buf = NULL;

void usage(char *command, int status) {
  fprintf(stderr, "Usage: %s userid [host] [port]\n", command);
  fprintf(stderr, "Example: sudo %s 1 172.16.0.116 %d\n", command,
          DEFAULT_PORT);
  exit(status);
}

void cleanup() {
  if (audio_info != NULL) {
    audio_free(audio_info);
  }
  if (buf != NULL) {
    free(buf);
  }
  if (sockfd != -1) {
    close(sockfd);
  }
}

void sigint_handler(int sig) {
  cleanup();
  exit(sig);
}

void send_udp_packets(uint32_t userid, in_addr_t host, uint16_t port) {
  int err;
  
  // Hardwired audio settings
  char *pcm_name = "hw:0,0";
  snd_pcm_stream_t stream = SND_PCM_STREAM_CAPTURE;
  int mode = 0;
  snd_pcm_format_t format = SND_PCM_FORMAT_S16_LE;
  uint8_t channels = 2;
  uint8_t sample_size_in_bytes = 2;
  uint8_t frame_size_in_bytes = channels * sample_size_in_bytes;
  uint32_t rate_in_hz = 48000;
  snd_pcm_uframes_t period_size_in_frames = 128;
  uint32_t period_size_in_bytes = period_size_in_frames * frame_size_in_bytes;
  uint8_t buffer_multiplicator = 10;

  // Handle Ctrl-c
  signal(SIGINT, sigint_handler);
  
  // Set scheduling parameters
  int priority;
  if ((priority = set_fifo_scheduling()) < 0) {
    perror("Could not set FIFO scheduling policy");
    exit(SCHED_ERROR);
  }
  fprintf(stderr, "FIFO scheduling policy has been set with priority %i\n",
          priority);
  
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
  if ((err = audio_new(pcm_name, stream, mode, format, channels, rate_in_hz,
                       sample_size_in_bytes, period_size_in_frames,
                       buffer_multiplicator, &audio_info)) < 0) {
    fprintf(stderr, "Could not initialize audio: %s\n", snd_strerror(err));
    exit(AUDIO_ERROR);
  }
  audio_print_parameters(audio_info);

  double period_size_in_ms =
    (double)period_size_in_frames / (rate_in_hz / 1000);
  fprintf(stderr, "Period size is %d bytes (%fms)\n", period_size_in_bytes,
          period_size_in_ms);
  uint32_t buf_size = HEADER_SIZE + period_size_in_bytes;
  buf = malloc(buf_size);

  uint32_t seqnum = 0;
  
  // Add userid to header
  memcpy(buf, &userid, sizeof(userid));
  /*
  buf[0] = userid & 0xff;
  buf[1] = (userid >> 8) & 0xff;
  buf[2] = (userid >> 16) & 0xff;
  buf[3] = (userid >> 24) & 0xff;
  */

  // Sender loop
  fprintf(stderr, "Sending audio...\n");
  while (true) {
    // Add sequence number and timestamp to header
    memcpy(&buf[4], &seqnum, sizeof(seqnum));
    /*
    buf[4] = seqnum & 0xff;
    buf[5] = (seqnum >> 8) & 0xff;
    buf[6] = (seqnum >> 16) & 0xff;
    buf[7] = (seqnum >> 24) & 0xff;
    */
    uint64_t timestamp = utimestamp();
    memcpy(&buf[8], &timestamp, sizeof(timestamp));
    /*
    buf[8] = timestamp & 0xff;
    buf[9] = (timestamp >> 8) & 0xff;
    buf[10] = (timestamp >> 16) & 0xff;
    buf[11] = (timestamp >> 24) & 0xff;
    */

    // Read audio data
    snd_pcm_uframes_t frames =
      snd_pcm_readi(audio_info->pcm, &buf[HEADER_SIZE],
                    period_size_in_frames);

    fprintf(stderr, "Failed to read from audio device: %s\n", snd_strerror(frames));
    
    if (frames == -EPIPE || frames == -ESTRPIPE) {
      fprintf(stderr, "Failed to read from audio device: %s\n", snd_strerror(frames));
      if (snd_pcm_recover(audio_info->pcm, frames, 0) < 0) {
        fprintf(stderr, "Failed to recover audio device: %s\n", snd_strerror(frames));
        break;
      }
    } else if (frames < 0) {
      fprintf(stderr, "Failed to read from audio device: %s\n", snd_strerror(frames));
      break;
    } else if (frames != period_size_in_frames) {
      fprintf(stderr, "Expected to read %ld frames from audio device but only read %ld\n",
              period_size_in_frames, frames);
      break;
    }
    
    // Send audio data:
    // |userid:4|seqnum:4|timestamp:8|data:period_size_in_bytes| = buf_size
    ssize_t bytes;
    if ((bytes = sendto(sockfd, &buf[HEADER_SIZE], period_size_in_bytes, 0,
                        (struct sockaddr *)&dest_addr,
                        sizeof(dest_addr))) < 0) {
      perror("Failed to write tosocket");
    } else if (bytes != period_size_in_bytes) {
      perror("Failed to write all bytes to socket");
      break;
    }

    seqnum++;
  }
  
  cleanup();
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
  
  send_udp_packets(userid, host, port);
  
  return 0;
}
