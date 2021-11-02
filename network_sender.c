#include <unistd.h>
#include <stdio.h>
#include <stdbool.h>
#include "audio.h"
#include "network_sender.h"
#include "timing.h"
#include "globals.h"

void *network_sender(void *arg) {
  int err;
  int sockfd = -1;
  uint8_t *udp_buf = NULL;
  audio_info_t *audio_info = NULL;
  
  // Extract parameters
  network_sender_params_t *sender_params = (network_sender_params_t *)arg;
  uint32_t userid = sender_params->userid;
  in_addr_t addr = sender_params->addr;
  uint16_t port = sender_params->port;
  
  // Create non-blocking socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket: Socket creation failed");
    exit(SOCKET_ERROR);
  }
  int flags = fcntl(sockfd, F_GETFL, 0);
  if (flags < 0) {
    perror("fcntl: Socket could not be made non-blocking");
    exit(SOCKET_ERROR);
  }
  if (fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) < 0) {
    perror("fcntl: Socket could not be made non-blocking");
    exit(SOCKET_ERROR);
  }
  
  struct sockaddr_in dest_addr = {0};
  dest_addr.sin_family = AF_INET;
  dest_addr.sin_port = htons(port);
  dest_addr.sin_addr.s_addr = addr;
  
  // Open audio device
  if ((err = audio_new(PCM_NAME, SND_PCM_STREAM_CAPTURE, SENDER_MODE, FORMAT,
                       CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                       SENDER_PERIOD_SIZE_IN_FRAMES,
                       SENDER_BUFFER_MULTIPLICATOR, &audio_info)) < 0) {
    fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
            snd_strerror(err));
    exit(AUDIO_ERROR);
  }
  audio_print_parameters(audio_info, "sender");
  assert(SENDER_PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);
  
  double period_size_in_ms =
    (double)SENDER_PERIOD_SIZE_IN_FRAMES / (RATE_IN_HZ / 1000);
  printf("Period size is %d bytes (%fms)\n", SENDER_PERIOD_SIZE_IN_BYTES,
         period_size_in_ms);
  
  uint32_t udp_buf_size = HEADER_SIZE + PAYLOAD_SIZE_IN_BYTES;
  udp_buf = malloc(udp_buf_size);

  // Add userid to buffer header
  memcpy(udp_buf, &userid, sizeof(userid));
  
  // Read from audio device and write to non blocking socket
  printf("Sending audio...\n");

  while (true) {
    bool give_up = false;

    // Add timestamp to buffer header
    uint64_t timestamp = utimestamp();
    memcpy(&udp_buf[4], &timestamp, sizeof(timestamp));
    
    // Read from audio device
    snd_pcm_uframes_t frames =
      snd_pcm_readi(audio_info->pcm, &udp_buf[HEADER_SIZE],
                    SENDER_PERIOD_SIZE_IN_FRAMES);
    if (frames == -EPIPE || frames == -ESTRPIPE) {
      fprintf(stderr, "snd_pcm_readi: Failed to read from audio device: %s\n",
              snd_strerror(frames));
      if (snd_pcm_recover(audio_info->pcm, frames, 0) < 0) {
        fprintf(stderr, "snd_pcm_readi: Failed to recover audio device: %s\n",
                snd_strerror(frames));
        continue;
      }
    } else if (frames < 0) {
      fprintf(stderr, "snd_pcm_readi: Failed to read from audio device: %s\n",
              snd_strerror(frames));
      break;
    } else if (frames != SENDER_PERIOD_SIZE_IN_FRAMES) {
      fprintf(stderr,
              "snd_pcm_readi Expected to read %d frames from audio device \
but only read %ld\n",
              SENDER_PERIOD_SIZE_IN_FRAMES, frames);
      break;
    }
    
    // Write to non-blocking socket
    uint32_t written_bytes = 0;
    while (written_bytes < udp_buf_size) {
      ssize_t n = sendto(sockfd, &udp_buf[written_bytes],
                         udp_buf_size - written_bytes,
                         0, (struct sockaddr *)&dest_addr, sizeof(dest_addr));
      if (n < 0) {
        if (errno == EWOULDBLOCK) {
          n = 0;
        } else {
          perror("sendto: Failed to write to socket");
          give_up = true;
          break;
        }
      }
      written_bytes += n;
    }

    if (give_up) {
      break;
    }
  }

  if (audio_info != NULL) {
    audio_free(audio_info);
  }
  if (udp_buf != NULL) {
    free(udp_buf);
  }
  if (sockfd != -1) {
    close(sockfd);
  }

  return NULL;
}
