#include <unistd.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/time.h>
#include <assert.h>
#include "timing.h"
#include "jb_table.h"
#include "network_receiver.h"
#include "globals.h"

#define FOUR_SECONDS_IN_US (4 * 1000000)
#define DRAIN_BUF_SIZE 1

extern jb_table_t *jb_table;

void *network_receiver(void *arg) {
  int sockfd = -1;

  printf("Jitter buffer contains %dms of audio data (%d periods, %d bytes)\n",
         JITTER_BUFFER_SIZE_IN_MS,
         PERIODS_IN_JITTER_BUFFER,
         JITTER_BUFFER_SIZE_IN_BYTES);

  // Extract parameters
  network_receiver_params_t *receiver_params = (network_receiver_params_t *)arg;
  uint16_t port = receiver_params->port;
  
  // Create and bind socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket: Socket creation failed");
    exit(SOCKET_ERROR);
  }
  
  struct sockaddr_in src_addr = {0};
  src_addr.sin_family = AF_INET;
  src_addr.sin_port = htons(port);
  src_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  
  if (bind(sockfd, (struct sockaddr *)&src_addr, sizeof(src_addr)) < 0) {
    perror("bind: Binding of socket failed");
    exit(SOCKET_ERROR);
  }
  
  struct timeval zero_timeout = {.tv_usec = 0, .tv_sec = 0};
  struct timeval one_second_timeout = {.tv_usec = 0, .tv_sec = 1};
  uint32_t udp_buf_size = HEADER_SIZE + PAYLOAD_SIZE_IN_BYTES;
  uint8_t drain_buf[DRAIN_BUF_SIZE];
    
  while (true) {
    // Waiting for incoming audio
    printf("Waiting for incoming audio...\n");
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(sockfd, &readfds);
    if (select(sockfd + 1, &readfds, 0, 0, NULL) < 0) {
      perror("select: Failed to wait for incoming audio");
      break;
    }
    
    // Drain socket receive buffer
    while (true) {
      FD_ZERO(&readfds);
      FD_SET(sockfd, &readfds);
      struct timeval timeout = zero_timeout;
      int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
      if (nfds < 0) {
        perror("select: Failed to drain socket receive buffer\n");
        goto bail_out;
      } else if (nfds == 0) {
        // Socket receiver buffer has been drained!
        break;
      }
      if (recvfrom(sockfd, drain_buf, DRAIN_BUF_SIZE, 0, NULL, NULL) < 0) {
        perror("recvfrom: Failed to drain socket receive buffer\n");
        goto bail_out;
      }
      FD_ZERO(&readfds);
      FD_SET(sockfd, &readfds);
    }
    printf("Socket receive buffer has been drained\n");
    
    // Read from socket and write to jitter buffer
    printf("Receiving audio...\n");
    uint64_t userid = 0;
    jb_t *jb = NULL;
    double latency = 0;
    uint64_t last_latency_printout = 0;        
    while (true) {
      // Wait for incoming socket data (or timeout)
      FD_ZERO(&readfds);
      FD_SET(sockfd, &readfds);
      struct timeval timeout = one_second_timeout;
      int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
      if (nfds < 0) {
        perror("select: Failed to wait for incoming socket data");
        goto bail_out;
      } else if (nfds == 0) {
        // Timeout
        printf("No longer receiving audio!\n");
        break;
      }
      
      // Peek into socket and extract userid
      uint32_t new_userid;
      int n;
      if ((n = recvfrom(sockfd, &new_userid, sizeof(uint32_t), MSG_PEEK, NULL,
                        NULL)) < 0) {
        perror("recvfrom: Failed to peek into socket and extract userid");
        goto bail_out;
      } else if (n != sizeof(uint32_t)) {
        printf("Ignored truncated UDP packet!\n");
        break;
      }
      
      // Get jitter buffer
      if (userid != new_userid) {
        jb_table_take_rdlock(jb_table);
        if ((jb = jb_table_find(jb_table, new_userid)) == NULL) {
          jb = jb_new(new_userid);
          assert(jb_table_add(jb_table, jb) == JB_TABLE_SUCCESS);
        }
        jb_table_release_lock(jb_table);
        userid = new_userid;
      }
      
      // Prepare new jitter buffer entry
      jb_entry_t *jb_entry;
      if (jb->entries > PERIODS_IN_JITTER_BUFFER) {
        jb_take_wrlock(jb);
        jb_entry = jb_pop(jb);
        jb_release_lock(jb);
        jb_entry->seqnum = 0;
      } else {
        jb_entry = jb_entry_new(udp_buf_size);
      }            
      
      // Read from socket
      if ((n = recvfrom(sockfd, jb_entry->data, udp_buf_size, 0, NULL,
                        NULL)) < 0) {
        perror("recvfrom: Failed to read from socket");
        goto bail_out;
      } else if (n != udp_buf_size) {
        printf("Ignored truncated UDP packet!\n");
        break;
      }
      
      // Calculate latency (for developement debugging only)
      uint64_t timestamp;
      memcpy(&timestamp, &jb_entry->data[4], sizeof(uint64_t));
      uint64_t now = utimestamp();
      // NOTE: Disable to allow development machines without NTP client
      //assert(now > timestamp);
      latency = latency * 0.9 + (now - timestamp) * 0.1;
      if (now - last_latency_printout > FOUR_SECONDS_IN_US) {
        // NOTE: Disable to remove noise on stdout
        //printf("Latency: %fms\n", latency / 1000);
        last_latency_printout = now;
      }
      
      // Add seqnum to jitter buffer entry and insert entry
      uint32_t seqnum;
      memcpy(&seqnum, &jb_entry->data[12], sizeof(seqnum));
      if (jb->entries > 0) {
        if (jb->tail->seqnum == seqnum) {
          printf("Duplicated UDP packet (%d)\n", seqnum);
        } else if (jb->tail->seqnum + 1 != seqnum) {
          printf("Missing UDP packet (%d)\n", jb->tail->seqnum + 1);
        }
      }
      jb_entry->seqnum = seqnum;

      jb_take_wrlock(jb);
      assert(jb_insert(jb, jb_entry) != 0);
      jb_release_lock(jb);      
    }

    jb_table_free(jb_table, true);
  }

 bail_out:
  fprintf(stderr, "network_receiver is shutting down!!!\n");
  close(sockfd);
  exit(NETWORK_RECEIVER_EXIT_STATUS);
  return NULL;
}
