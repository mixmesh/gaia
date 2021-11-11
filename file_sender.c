#include <stdbool.h>
#include "audio.h"
#include "globals.h"
#include "timing.h"
#include "file_sender.h"
#include "audio.h"

void *file_sender(void *arg) {
  // Extract parameters
  file_sender_params_t *sender_params = (file_sender_params_t *)arg;
  uint32_t userid = sender_params->userid;
  //char *filename = sender_params->filename;
  uint8_t naddr_ports = sender_params->naddr_ports;
  file_sender_addr_port_t *addr_ports = sender_params->addr_ports;

  // Open file
  
  
  
  // Create socket
  int sockfd = -1;
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket: Socket creation failed");
    exit(SOCKET_ERROR);
  }
  
  // Resize socket send buffer
  /*
  // NOTE: This was a bad idea for some reason. Disable for now.
  int snd_buf_size = PERIOD_SIZE_IN_BYTES * BUFFER_MULTIPLICATOR;
  assert(setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &snd_buf_size,
                    sizeof(snd_buf_size)) == 0);
  */
  
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
  struct sockaddr_in addrs[naddr_ports];
  for (int i = 0; i < naddr_ports; i++) {
    memset(&addrs[i], 0, sizeof(addrs[i]));
    addrs[i].sin_family = AF_INET;
    addrs[i].sin_addr.s_addr = addr_ports[i].addr;
    addrs[i].sin_port = htons(addr_ports[i].port);
  }
    
  uint32_t udp_buf_size = HEADER_SIZE + PAYLOAD_SIZE_IN_BYTES;
  uint8_t *udp_buf = malloc(udp_buf_size);
  uint32_t seqnum = 1;
  
  // Add userid to buffer header
  memcpy(udp_buf, &userid, sizeof(userid));

  // Read from file and write to socket
  printf("Sending audio...\n");  
  while (true) {
    // Add timestamp to buffer header
    uint64_t timestamp = utimestamp();
    memcpy(&udp_buf[4], &timestamp, sizeof(timestamp));
    
    // Add seqnum to buffer header
    memcpy(&udp_buf[12], &seqnum, sizeof(seqnum));
    seqnum++;
    
    // Read from file;
    // FIXME
    
    // Write to non-blocking socket
    snd_pcm_uframes_t frames = 0;
    if (frames == PERIOD_SIZE_IN_FRAMES) {
      for (int i = 0; i < naddr_ports; i++) {
        ssize_t n  = sendto(sockfd, udp_buf, udp_buf_size, 0,
                            (struct sockaddr *)&addrs[i], sizeof(addrs[i]));
        if (n < 0) {
          perror("sendto: Failed to write to socket");
        } else if (n != udp_buf_size) {
          printf("Too few bytes written to socket!\n");
        }
      }
    } else {
      printf("Too few frames read from file!\n");
    }
  }
  
  fprintf(stderr, "network_sender is shutting down!!!\n");
  free(udp_buf);
  close(sockfd);
  exit(NETWORK_SENDER_EXIT_STATUS);
  return NULL;
}
