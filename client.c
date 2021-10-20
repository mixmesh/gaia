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
#include "globals.h"
#include "audio.h"
#include "timing.h"

#define SOCKET_ERROR 1

void usage(char *command, int status) {
  fprintf(stderr, "Usage: %s userid [host] [port]\n", command);
  exit(status);
}

audio_info_t *audio_info = NULL;
int sockfd = -1;

void sigint(int sig) {
  if (audio_info != NULL) {
    audio_free(audio_info);
  }
  if (sockfd != -1) {
    close(sockfd);
  }
  exit(1);
}

void start_client(uint32_t userid, in_addr_t host, uint16_t port) {
  // Handle Ctrl-c
  signal(SIGINT, sigint);
  // Create socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket creation failed");
    exit(SOCKET_ERROR);
  }
  struct sockaddr_in servaddr;
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = host;
  servaddr.sin_port = htons(port);
  // Open ALSA device
  fprintf(stderr, "0\n");
  int err;
  if ((err = audio_new(SND_PCM_STREAM_CAPTURE, &audio_info)) < 0) {
    fprintf(stderr, "could not initialize audio: %s\n", snd_strerror(err));
    exit(1);
  }
  fprintf(stderr, "0a\n");
  audio_print_parameters(audio_info);
  // Send loop
  fprintf(stderr, "2\n");
  int udp_buf_size = HEADER_SIZE + audio_info->period_size_in_bytes;
  fprintf(stderr, "3\n");
  uint8_t udp_buf[udp_buf_size];
  fprintf(stderr, "UUU: %d\n", userid);
  udp_buf[0] = userid & 0xff;
  fprintf(stderr, "3\n");
  udp_buf[1] = (userid >> 8) & 0xff;
  fprintf(stderr, "3a\n");
  udp_buf[2] = (userid >> 16) & 0xff;
  fprintf(stderr, "3b\n");
  udp_buf[3] = (userid >> 24) & 0xff;
  uint32_t index = 0;
  fprintf(stderr, "4\n");
  while (true) {
    fprintf(stderr, "1\n");
    // Prepare header
    udp_buf[4] = index & 0xff;
    udp_buf[5] = (index >> 8) & 0xff;
    udp_buf[6] = (index >> 16) & 0xff;
    udp_buf[7] = (index >> 24) & 0xff;
    uint32_t timestamp = get_timestamp();
    udp_buf[8] = timestamp & 0xff;
    udp_buf[9] = (timestamp >> 8) & 0xff;
    udp_buf[10] = (timestamp >> 16) & 0xff;
    udp_buf[11] = (timestamp >> 24) & 0xff;
    // Read audio data
    int err = snd_pcm_readi(audio_info->handle, &udp_buf[12],
                            audio_info->period_size_in_frames);
    if (err == -EPIPE) {
      fprintf(stderr, "overrun occurred\n");
      snd_pcm_prepare(audio_info->handle);
    } else if (err < 0) {
      fprintf(stderr, "error from read: %s\n", snd_strerror(err));
    } else if (err != audio_info->period_size_in_frames) {
      fprintf(stderr, "short read, read %d frames\n", err);
    }
    // Write UDP packet    
    ssize_t n = sendto(sockfd, udp_buf, udp_buf_size, 0,
                       (struct sockaddr *)&servaddr,
                       sizeof(servaddr));
    assert(n == udp_buf_size);
    fprintf(stderr, "+");
    ++index;
  }
}

int main (int argc, char *argv[]) {
  if (argc < 2 || argc > 4) {
    usage(argv[0], 1);
  }
  char *endptr;
  long userid = strtol(argv[1], &endptr, 10);
  if (strlen(endptr) != 0) {
    usage(argv[0], 3);
  }
  fprintf(stderr, "userid: %ld\n", userid);
  in_addr_t host = inet_addr(DEFAULT_HOST);
  uint16_t port = DEFAULT_PORT;
  if (argc > 2) {
    if ((host = inet_addr(argv[2])) == -1) {
      usage(argv[0], 2);
    }    
    if (argc > 3) {
      port = strtol(argv[3], &endptr, 10);
      if (strlen(endptr) != 0) {
        usage(argv[0], 3);
      }
    }
  }
  start_client(userid, host, port);
  return 0;
}
