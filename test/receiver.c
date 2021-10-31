#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sched.h>
#include "../network_receiver.h"

#define DEFAULT_PORT 2305

#define SCHED_ERROR -1
#define ARG_ERROR -2

void usage(char *argv[]) {
  fprintf(stderr, "Usage: %s [port]\n", argv[0]);
  fprintf(stderr, "  Example: sudo %s %d\n", argv[0], DEFAULT_PORT);
  exit(ARG_ERROR);
}

int main (int argc, char *argv[]) {
  if (argc > 2) {
    usage(argv);
  }
  
  // Read port
  uint32_t port = DEFAULT_PORT;
  if (argc > 3) {
    char *endptr;
    port = strtol(argv[1], &endptr, 10);
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

  // Start network receiver
  network_receiver_params_t receiver_params =
    {
     .addr = -1, // not used yet
     .port = port
    };
  network_receiver((void *)&receiver_params);
  
  return 0;
}
