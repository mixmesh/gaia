#include <unistd.h>
#include <stdio.h>
#include "network_sender.h"
#include "network_receiver.h"
#include "audio_sink.h"
#include "jb_table.h"
#include "timing.h"

#define SRC_ADDR "127.0.0.1"
#define SRC_PORT 2305
#define DEST_ADDR "127.0.0.1"
#define DEST_PORT 2305

#define ARG_ERROR -1
#define SCHED_ERROR -2
#define THREAD_ERROR -3

#define MAX_NETWORK_SENDER_ADDR_PORTS 256

jb_table_t *jb_table;

void usage(char *argv[]) {
  fprintf(stderr, "Usage: %s [-s addr[:port]] [-d addr[:port]] userid\n",
          argv[0]);
  fprintf(stderr, "  Note: addr and port default to 127.0.0.1 and 2305\n");
  fprintf(stderr,
          "  Example: sudo %s -s 172.16.0.116:2305 -d 172.16.0.95 -d \
172.16.0.95:2356 4711\n",
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
    long value;
    if (string_to_long(token, &value) < 0) {
      return -1;
    }
    *port = value;
  }
  if (strtok(NULL, ":") != NULL) {
    return -1;
  }
  return 0;
}

int set_fifo_scheduling(pthread_attr_t *attr, int8_t priority_offset) {
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
  sched_param.sched_priority = sched_get_priority_max(SCHED_FIFO) +
    priority_offset;  
  if ((err = pthread_attr_setschedparam(attr, &sched_param)) != 0) {
    return err;
  }
  return 0;
}

int main (int argc, char *argv[]) {
  int err;

  in_addr_t src_addr = inet_addr(SRC_ADDR);
  uint16_t src_port = SRC_PORT;

  network_sender_addr_port_t dest_addr_ports[MAX_NETWORK_SENDER_ADDR_PORTS];
  dest_addr_ports[0].addr = inet_addr(DEST_ADDR);
  dest_addr_ports[0].port = DEST_PORT;

  int opt, ndest_addr_ports = 0;

  while ((opt = getopt(argc, argv, "s:d:")) != -1) {
    switch (opt) {
    case 's':
      if (get_addr_port(optarg, &src_addr, &src_port) < 0) {
        usage(argv);
      }
      break;
    case 'd':
      if (get_addr_port(optarg, &dest_addr_ports[ndest_addr_ports].addr,
                        &dest_addr_ports[ndest_addr_ports].port) < 0) {
        usage(argv);
      }
      ndest_addr_ports = (ndest_addr_ports + 1) % MAX_NETWORK_SENDER_ADDR_PORTS;
      break;
    default:
      usage(argv);
    }
  }

  if (ndest_addr_ports == 0) {
    ndest_addr_ports = 1;
  }

  if (optind != argc - 1) {
    usage(argv);
  }

  uint32_t userid;
  long value;
  if (string_to_long(argv[optind], &value) < 0) {
    usage(argv);
  }
  userid = value;
  
  if (userid == 0) {
    usage(argv);
  }
  
  jb_table = jb_table_new();
  
  // Start sender thread
  pthread_t sender_thread;
  network_sender_params_t sender_params =
    {
     .userid = userid,
     .naddr_ports = ndest_addr_ports,
     .addr_ports = dest_addr_ports
    };
  pthread_attr_t sender_attr;
  if ((err = set_fifo_scheduling(&sender_attr, 0)) != 0) {
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
    };
  pthread_attr_t receiver_attr;
  if ((err = set_fifo_scheduling(&receiver_attr, 0)) != 0) {
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

  msleep(500);
  
  // Start audio sink thread
  pthread_t audio_sink_thread;
  audio_sink_params_t audio_sink_params;
  pthread_attr_t audio_sink_attr;
  if ((err = set_fifo_scheduling(&audio_sink_attr, 0)) != 0) {
    fprintf(stderr, "pthread_create: Scheduling could not be configured (%d)\n",
            err);
    exit(SCHED_ERROR);
  }
  if ((err = pthread_create(&audio_sink_thread, &audio_sink_attr, audio_sink,
                            (void *)&audio_sink_params)) < 0) {
    fprintf(stderr,
            "pthread_create: Failed to start network audio sink thread (%d)\n",
            err);
    exit(THREAD_ERROR);
  }

  // This will block until all threads have terminated or until any thread
  // thread calls exit(3)
  pthread_exit(0);

  jb_table_free(jb_table, false);
  
  return 0;
}
