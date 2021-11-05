#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <string.h>
#include <inttypes.h>
#include <pthread.h>
#include "network_sender.h"
#include "network_receiver.h"
#include "jb_table.h"

#define SRC_ADDR "127.0.0.1"
#define SRC_PORT 2305
#define DEST_ADDR "127.0.0.1"
#define DEST_PORT 2305

#define ARG_ERROR -1
#define SCHED_ERROR -2
#define THREAD_ERROR -3

jb_t *jb_table = NULL;

void usage(char *argv[]) {
  fprintf(stderr, "Usage: %s [-s addr[:port]] [-d addr[:port]] userid\n",
          argv[0]);
  fprintf(stderr, "  Note: addr and port default to 127.0.0.1 and 2305\n");
  fprintf(stderr,
          "  Example: sudo %s -s 172.16.0.116:2305 -d 172.16.0.95 4711\n",
          argv[0]);
  exit(ARG_ERROR);
}

int string_to_long(char *string, long *value) {
  char *endptr;
  *value = strtol(string, &endptr, 10);
  if (strlen(endptr) != 0) {
    return -1;
  }
  return 0;
}

int get_addr_port(char *arg, in_addr_t *addr, uint16_t *port) {
  char *token;
  token = strtok(arg, ":");
  if ((*addr = inet_addr(token)) == -1) {
    return -1;
  }
  if ((token = strtok(NULL, ":")) != NULL) {
    if (string_to_long(token, (long *)port) < 0) {
      return -1;
    }
  }
  if (strtok(NULL, ":") != NULL) {
    return -1;
  }
  return 0;
}

int set_fifo_scheduling(pthread_attr_t *attr) {
  int err;
  if ((err = pthread_attr_init(attr)) != 0) {
    return err;
  }
  struct sched_param sched_param;
  if ((err = pthread_attr_getschedparam(attr, &sched_param)) != 0) {
    return err;
  }
  if ((err = pthread_attr_setinheritsched(attr, PTHREAD_EXPLICIT_SCHED)) != 0) {
    return err;
  }
  if ((err = pthread_attr_setschedpolicy(attr, SCHED_FIFO)) != 0) {
    return err;
  }
  sched_param.sched_priority = sched_get_priority_max(SCHED_FIFO);  
  if ((err = pthread_attr_setschedparam(attr, &sched_param)) != 0) {
    return err;
  }
  return 0;
}

int main (int argc, char *argv[]) {
  int err;
  in_addr_t src_addr = inet_addr(SRC_ADDR);
  uint16_t src_port = SRC_PORT;
  in_addr_t dest_addr = inet_addr(DEST_ADDR);
  uint16_t dest_port = DEST_PORT;

  int opt;
  while ((opt = getopt(argc, argv, "s:d:")) != -1) {
    switch (opt) {
    case 's':
      if (get_addr_port(optarg, &src_addr, &src_port) < 0) {
        usage(argv);
      }
      break;
    case 'd':
      if (get_addr_port(optarg, &dest_addr, &dest_port) < 0) {
        usage(argv);
      }
      break;
    default:
      usage(argv);
    }
  }

  if (optind != argc - 1) {
    usage(argv);
  }

  uint32_t userid;
  if (string_to_long(argv[optind], (long *)&userid) < 0) {
    usage(argv);
  }
  if (userid == 0) {
    usage(argv);
  }
  
  // Start sender thread
  pthread_t sender_thread;
  network_sender_params_t sender_params =
    {
     .userid = userid,
     .addr = dest_addr,
     .port = dest_port
    };
  pthread_attr_t sender_attr;
  if ((err = set_fifo_scheduling(&sender_attr)) != 0) {
    fprintf(stderr,
            "set_fifo_scheduling: Scheduling could not be configured (%d)\n",
            err);
    exit(SCHED_ERROR);
  }
  if ((err = pthread_create(&sender_thread, &sender_attr, network_sender,
                            (void *)&sender_params)) < 0) {
    fprintf(stderr,
            "pthread_create: Failed to start network sender thread (%d)\n",
            err);
    exit(THREAD_ERROR);
  }

  // Start receiver thread
  pthread_t receiver_thread;
  network_receiver_params_t receiver_params =
    {
     .addr = src_addr,
     .port = src_port,
     .audio_sink = true
    };
  pthread_attr_t receiver_attr;
  if ((err = set_fifo_scheduling
       (&receiver_attr)) != 0) {
    fprintf(stderr, "pthread_create: Scheduling could not be configured (%d)\n",
            err);
    exit(SCHED_ERROR);
  }
  if ((err = pthread_create(&receiver_thread, &receiver_attr, network_receiver,
                            (void *)&receiver_params)) < 0) {
    fprintf(stderr,
            "pthread_create: Failed to start network receiver thread (%d)\n",
            err);
    exit(THREAD_ERROR);
  }
  
  pthread_exit(0); // This will block until all threads have terminated

  return 0;
}
