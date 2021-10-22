#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include <assert.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include "jb.h"
#include "audio.h"
#include "timing.h"

#define DEFAULT_HOST "127.0.0.1"
#define DEFAULT_PORT 54382

// |userid:4|seqnum:4|timestamp:4|
#define HEADER_SIZE (4 + 4 + 4)

#define SOCKET_ERROR 1

void usage(char *command, int status) {
  fprintf(stderr, "Usage: %s userid (not 0) [host] [port]\n", command);
  exit(status);
}

audio_info_t *audio_info = NULL;
int sockfd = -1;
uint8_t *udp_buf = NULL;

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
  exit(1);
}

void start_client(uint32_t userid, in_addr_t host, uint16_t port) {
  // Handle Ctrl-c
  signal(SIGINT, sigint);
  // Open audio device
  int err;
  if ((err = audio_new("default", SND_PCM_STREAM_CAPTURE,
                       SND_PCM_FORMAT_MU_LAW, 1, 8000, 1, 100,
                       3, &audio_info)) < 0) {
    fprintf(stderr, "could not initialize audio: %s\n", snd_strerror(err));
    exit(1);
  }
  audio_print_info(audio_info);
  // Create socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket creation failed");
    exit(SOCKET_ERROR);
  }
  int n = audio_info->period_size_in_bytes;
  if ((err = setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &n, sizeof(n))) < 0) {
    perror("send buffer could not be resized");
    exit(SOCKET_ERROR);
  }
  struct sockaddr_in servaddr;
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = host;
  servaddr.sin_port = htons(port);
  // Send loop
  uint32_t udp_buf_size = HEADER_SIZE + audio_info->period_size_in_bytes;
  udp_buf = malloc(udp_buf_size);
  udp_buf[0] = userid & 0xff;
  udp_buf[1] = (userid >> 8) & 0xff;
  udp_buf[2] = (userid >> 16) & 0xff;
  udp_buf[3] = (userid >> 24) & 0xff;
  uint32_t seqnum = 0;
  while (true) {
    // Prepare header
    udp_buf[4] = seqnum & 0xff;
    udp_buf[5] = (seqnum >> 8) & 0xff;
    udp_buf[6] = (seqnum >> 16) & 0xff;
    udp_buf[7] = (seqnum >> 24) & 0xff;
    uint32_t timestamp = get_timestamp();
    udp_buf[8] = timestamp & 0xff;
    udp_buf[9] = (timestamp >> 8) & 0xff;
    udp_buf[10] = (timestamp >> 16) & 0xff;
    udp_buf[11] = (timestamp >> 24) & 0xff;
    // Read audio data
    int err = snd_pcm_readi(audio_info->handle, &udp_buf[HEADER_SIZE],
                            audio_info->period_size_in_frames);
    if (err == -EPIPE) {
      fprintf(stderr, "overrun occurred\n");
      snd_pcm_prepare(audio_info->handle);
    } else if (err < 0) {
      fprintf(stderr, "error from read: %s\n", snd_strerror(err));
    } else if (err != audio_info->period_size_in_frames) {
      fprintf(stderr, "short read, read %d frames\n", err);
    }
    // Write UDP packet:
    // |userid:4|seqnum:4|timestamp:4|audio:period_size_in_bytes| = udp_buf_size
    ssize_t sent_bytes;
    if ((sent_bytes = sendto(sockfd, udp_buf, udp_buf_size, 0,
                             (struct sockaddr *)&servaddr,
                             sizeof(servaddr))) < 0) {
      perror("all bytes could not be sent");
    }
    //printf("+");
    ++seqnum;
  }
}

int main (int argc, char *argv[]) {
  if (argc < 2 || argc > 4) {
    usage(argv[0], 1);
  }
  char *endptr;
  uint32_t userid = strtol(argv[1], &endptr, 10);
  if (strlen(endptr) != 0) {
    usage(argv[0], 2);
  }
  if (userid == 0) {
    usage(argv[0], 3);
  }
  in_addr_t host = inet_addr(DEFAULT_HOST);
  uint16_t port = DEFAULT_PORT;
  if (argc > 2) {
    if ((host = inet_addr(argv[2])) == -1) {
      usage(argv[0], 4);
    }    
    if (argc > 3) {
      port = strtol(argv[3], &endptr, 10);
      if (strlen(endptr) != 0) {
        usage(argv[0], 5);
      }
    }
  }
  start_client(userid, host, port);
  return 0;
}
