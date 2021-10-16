#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include "jb.h"
#include "globals.h"

#define SOCKET_ERROR 1

void usage(char *command, uint16_t status) {
  fprintf(stderr, "Usage: %s name [host] [port]\n", command);
  exit(status);
}

void msleep(long ms) {
  struct timespec req = 
    {
     .tv_sec = ms / 1000,
     .tv_nsec = (ms % 1000) * 1000000L
    };
  nanosleep(&req, NULL);
}

void start_client(char *name, in_addr_t host, uint16_t port) {
  // Create socket
  int sockfd;
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket creation failed");
    exit(SOCKET_ERROR);
  }
  struct sockaddr_in servaddr;
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = host;
  servaddr.sin_port = htons(port);
  // Send loop
  uint8_t buf[UDP_BUF_SIZE];
  strcpy((char *)buf, name); 
  uint32_t index = 0;
  while (true) {
    // Send |name:32|index:4|data:DATA_SIZE| = UDP_BUF_SIZE bytes
    buf[32] = index & 0xff;
    buf[33] = (index >> 8) & 0xff;
    buf[34] = (index >> 16) & 0xff;
    buf[35] = (index >> 24) & 0xff;
    ssize_t n = sendto(sockfd, buf, UDP_BUF_SIZE, 0,
                       (struct sockaddr *)&servaddr,
                       sizeof(servaddr));
    assert(n == UDP_BUF_SIZE);
    fprintf(stderr, "+");
    msleep(10);
    ++index;
  }
}

int main (int argc, char *argv[]) {
  if (argc < 2 || argc > 4) {
    usage(argv[0], 1);
  }
  char *name = argv[1];
  in_addr_t host = inet_addr(DEFAULT_HOST);
  uint16_t port = DEFAULT_PORT;
  if (argc > 2) {
    if ((host = inet_addr(argv[2])) == -1) {
      usage(argv[0], 2);
    }    
    if (argc > 3) {
      char *endptr;
      port = strtol(argv[3], &endptr, 10);
      if (strlen(endptr) != 0) {
        usage(argv[0], 3);
      }
    }
  }
  start_client(name, host, port);
  return 0;
}
