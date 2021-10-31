#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <sched.h>
#include "../network_sender.h"

#define DEFAULT_USERID 1
#define DEFAULT_ADDR "127.0.0.1"
#define DEFAULT_PORT 2305

#define SCHED_ERROR -1
#define ARG_ERROR -2

void usage(char *argv[]) {
  fprintf(stderr, "Usage: %s [userid] [addr] [port]\n", argv[0]);
  fprintf(stderr, "  Example: sudo %s 1 172.16.0.116 %d\n", argv[0],
          DEFAULT_PORT);
  exit(ARG_ERROR);
}

int main (int argc, char *argv[]) {
  if (argc < 1 || argc > 4) {
    usage(argv);
  }
  
  char *endptr;
  
  // Read userid
  uint32_t userid = DEFAULT_USERID;
  if (argc > 1) {
    userid = strtol(argv[1], &endptr, 10);
    if (strlen(endptr) != 0 && userid != 0) {
      usage(argv);
    }
  }
  
  // Read addr
  in_addr_t addr = inet_addr(DEFAULT_ADDR);
  if (argc > 2) {
    if ((addr = inet_addr(argv[2])) == -1) {
      usage(argv);
    }
  }
  
  // Read port
  uint32_t port = DEFAULT_PORT;
  if (argc > 3) {
    port = strtol(argv[3], &endptr, 10);
    if (strlen(endptr) != 0) {
      usage(argv);
    }
  }

  // Set scheduling policy
  struct sched_param sched_param;
  int err;
  if ((err = sched_getparam(0, &sched_param)) < 0) {
    return err;
  }
  sched_param.sched_priority = sched_get_priority_max(SCHED_FIFO);
  if ((err = sched_setscheduler(0, SCHED_FIFO, &sched_param)) < 0) {
    return err;
  } 

  // Start network sender
  network_sender_params_t sender_params =
    {
     .userid = userid,
     .addr = addr,
     .port = port
    };
  network_sender((void *)&sender_params);

  return 0;
}
