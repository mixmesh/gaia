#include <stdbool.h>
#include "audio.h"
#include "globals.h"
#include "timing.h"
#include "network_sender.h"
#include "audio.h"

void *network_sender(void *arg) {
  int err;
  
  // Extract parameters
  network_sender_params_t *sender_params = (network_sender_params_t *)arg;
  uint32_t userid = sender_params->userid;
  uint8_t naddr_ports = sender_params->naddr_ports;
  network_sender_addr_port_t *addr_ports = sender_params->addr_ports;
  
  // Create socket
  int sockfd = -1;
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
  
  // Create destination addresses
  struct sockaddr_in dest_addrs[naddr_ports];
  for (int i = 0; i < naddr_ports; i++) {
    memset(&dest_addrs[i], 0, sizeof(dest_addrs[i]));
    dest_addrs[i].sin_family = AF_INET;
    dest_addrs[i].sin_addr.s_addr = addr_ports[i].addr;
    dest_addrs[i].sin_port = htons(addr_ports[i].port);
  }
  
  // Open audio device
  audio_info_t *audio_info;
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

  uint8_t *udp_buf = malloc(udp_buf_size);
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
      for (int i = 0; i < naddr_ports; i++) {
        ssize_t n  = sendto(sockfd, udp_buf, udp_buf_size, 0,
                            (struct sockaddr *)&dest_addrs[i], sizeof(dest_addrs[i]));
        if (n < 0) {
          perror("sendto: Failed to write to socket");
        } else if (n != udp_buf_size) {
          printf("Too few bytes written to socket!\n");
        }
      }
    } else {
      printf("Too few frames read from audio device!\n");
    }
  }
  
  fprintf(stderr, "network_sender is shutting down!!!\n");
  audio_free(audio_info);
  free(udp_buf);
  close(sockfd);
  exit(NETWORK_SENDER_EXIT_STATUS);
  return NULL;
}
