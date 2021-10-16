#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <assert.h>
#include "jb.h"
#include "jb_table.h"
#include "globals.h"
#include "bits.h"

#define SOCKET_ERROR 1
#define BIND_ERROR 2
#define MAX_JITTER_BUFFER_ENTRIES 20

void usage(char *command, uint16_t status) {
  fprintf(stderr, "Usage: %s [port]\n", command);
  exit(status);
}

void start_server(uint16_t port) {
  // Create socket
  int sockfd;
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket creation failed");
    exit(SOCKET_ERROR);
  }
  struct sockaddr_in servaddr;
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = INADDR_ANY;
  servaddr.sin_port = htons(port);
  // Bind socket
  if (bind(sockfd, (const struct sockaddr *)&servaddr,
           sizeof(servaddr)) < 0) {
    perror("bind failed");
    exit(BIND_ERROR);
  }
  // Receive loop
  char name[32] = {0};
  jb_t *jb = NULL;
  jb_t *jb_table = jb_table_new();
  uint8_t buf[UDP_BUF_SIZE];
  while (true) {
    // Receive |name:32|index:4|data:DATA_SIZE| = UDP_BUF_SIZE bytes
    ssize_t n = recvfrom(sockfd, buf, UDP_BUF_SIZE, 0, NULL, NULL);
    assert(n == UDP_BUF_SIZE);
    // Get jitter buffer (create if needed)    
    if (jb == NULL || strcmp((char *)buf, name) != 0) {
      memcpy(name, buf, 32);      
      if ((jb = jb_table_find(&jb_table, name)) == NULL) {
        jb = jb_new(name);
        jb_table_add(&jb_table, jb);
      }
    }
    // Insert jitter buffer entry
    uint32_t index =
      buf[32] + (buf[33] << 8) + (buf[34] << 16) + (buf[35] << 24);
    if (jb->tail == NULL || index > jb->tail->index ) {
      // Prepare new jitter buffer entry
      jb_entry_t *jb_entry;
      if (jb->entries == MAX_JITTER_BUFFER_ENTRIES) {
        jb_entry = jb_pop(jb);
      } else {
        jb_entry = jb_entry_new(DATA_SIZE);
      }
      jb_entry->index = index;
      memcpy(jb_entry->data, &buf[HEADER_SIZE], DATA_SIZE);
      // Insert!
      uint8_t result = jb_insert(jb, jb_entry);
      assert(CHK_FLAG(result,
                      HEAD_INSERTED|TAIL_INSERTED|INTERMEDIATE_INSERTED|
                      FIRST_INSERTED));
      if (CHK_FLAG(result, ALREADY_EXISTS)) {
        jb_entry_free(jb_entry);
        fprintf(stderr, "D");
      } else if (CHK_FLAG(result, TAIL_INSERTED)) {
        fprintf(stderr, ">");
      } else if (CHK_FLAG(result, HEAD_INSERTED)) {
        //fprintf(stderr, "<");
      } else if (CHK_FLAG(result, FIRST_INSERTED)) {
        fprintf(stderr, "0");
      } else if (CHK_FLAG(result, INTERMEDIATE_INSERTED)) {
        fprintf(stderr, ".");
      } else {
        fprintf(stderr, "?");
      }
    } else {
      fprintf(stderr, "I");
    }
  }
}

int main (int argc, char *argv[]) {
  if (argc > 2) {
    usage(argv[0], 1);
  }
  uint16_t port = DEFAULT_PORT;
  if (argc == 2) {
    char *endptr;
    port = strtol(argv[1], &endptr, 10);
    if (strlen(endptr) != 0) {
      usage(argv[0], 2);
    }
  }
  start_server(port);
  return 0;
}
