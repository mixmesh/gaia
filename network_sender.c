#include <stdbool.h>
#include "audio.h"
#include "globals.h"
#include "timing.h"
#include "network_sender.h"
#include "audio.h"

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
  
  // Create socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket: Socket creation failed");
    exit(SOCKET_ERROR);
  }

  // Resize socket send buffer to eight periods
  int snd_buf_size = PERIOD_SIZE_IN_BYTES * 8;
  assert(setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &snd_buf_size,
                    sizeof(snd_buf_size)) == 0);
  
  // Make socket non-blocking
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
  if ((err = audio_new(PCM_NAME, SND_PCM_STREAM_CAPTURE, 0, FORMAT,
                       CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                       PERIOD_SIZE_IN_FRAMES, BUFFER_MULTIPLICATOR,
                       &audio_info)) < 0) {
    fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
            snd_strerror(err));
    exit(AUDIO_ERROR);
  }
  audio_print_parameters(audio_info, "sender");
  assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);
  
  printf("Period size is %d bytes (%fms)\n", PERIOD_SIZE_IN_BYTES,
         PERIOD_SIZE_IN_MS);
  
  uint32_t udp_buf_size = HEADER_SIZE + PAYLOAD_SIZE_IN_BYTES;
  udp_buf = malloc(udp_buf_size);
  uint32_t seqnum = 1;
  
  // Add userid to buffer header
  memcpy(udp_buf, &userid, sizeof(userid));

  // Read from audio device and write to socket
  printf("Sending audio...\n");  
  while (true) {
    // Add timestamp to buffer header
    uint64_t timestamp = utimestamp();
    memcpy(&udp_buf[4], &timestamp, sizeof(timestamp));
    
    // Add seqnum to buffer header
    memcpy(&udp_buf[12], &seqnum, sizeof(seqnum));
    seqnum++;
    
    // Read from audio device
    snd_pcm_uframes_t frames;
    if ((frames = audio_read(audio_info, &udp_buf[HEADER_SIZE],
                             PERIOD_SIZE_IN_FRAMES)) < 0) {      
      continue;
    }
    
    // Write to non-blocking socket
    if (frames == PERIOD_SIZE_IN_FRAMES) {
      uint32_t written_bytes = 0;
      while (written_bytes < udp_buf_size) {
        ssize_t n = sendto(sockfd, udp_buf, udp_buf_size - written_bytes, 0,
                           (struct sockaddr *)&dest_addr, sizeof(dest_addr));
        if (n < 0) {
          perror("sendto: Failed to write to socket");
          break;
        }
        written_bytes += n;
      }
    } else {
      printf("Too few frames read from audio device. Ignore them!\n");
    }
  }
  
  fprintf(stderr, "network_sender is shutting down!!!\n");
  audio_free(audio_info);
  free(udp_buf);
  close(sockfd);
  exit(NETWORK_SENDER_EXIT_STATUS);
  return NULL;
}
