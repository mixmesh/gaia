#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include<sys/stat.h>
#include "file_sender.h"
#include "gaia_utils.h"
#include "globals.h"

#define MAX_FILE_SENDER_ADDR_PORTS 256

void usage(char *argv[]) {
    fprintf(stderr, "Usage: %s [-d addr[:port]] userid filename\n",
            argv[0]);
    fprintf(stderr,
            "Note: addr and port default respectively defaults to %s and %d\n",
            DEFAULT_ADDR, DEFAULT_PORT);
    fprintf(stderr,
            "Example: sudo %s -d 172.16.0.95 -d 172.16.0.95:2356 4711 \
manhattan.u16\n",
            argv[0]);
    exit(ARG_ERROR);
}

int main (int argc, char *argv[]) {
    int err;

    file_sender_addr_port_t addr_ports[MAX_FILE_SENDER_ADDR_PORTS];
    addr_ports[0].addr = inet_addr(DEFAULT_ADDR);
    addr_ports[0].port = DEFAULT_PORT;

    int opt, naddr_ports = 0;

    while ((opt = getopt(argc, argv, "d:")) != -1) {
        switch (opt) {
        case 'd':
            if (get_addr_port(optarg, &addr_ports[naddr_ports].addr,
                              &addr_ports[naddr_ports].port) < 0) {
                usage(argv);
            }
            naddr_ports = (naddr_ports + 1) % MAX_FILE_SENDER_ADDR_PORTS;
            break;
        default:
            usage(argv);
        }
    }

    if (naddr_ports == 0) {
        naddr_ports = 1;
    }

    if (argc != 3) {
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

    char *filename = argv[optind + 1];
    struct stat buffer;
    if (stat(filename,&buffer) != 0) {
        usage(argv);
    }

    // Start sender thread
    pthread_t sender_thread;
    file_sender_params_t sender_params =
        {
         .userid = userid,
         .filename = filename,
         .naddr_ports = naddr_ports,
         .addr_ports = addr_ports
        };

    pthread_attr_t sender_attr;
    if ((err = pthread_attr_init(&sender_attr)) != 0) {
        fprintf(stderr,
                "pthread_attr_init: Failed to initialize sender thread \
attribute (%d)\n",
                err);
        exit(THREAD_ERROR);
    }

    if (geteuid() == 0) {
        pthread_attr_t sender_attr;
        if ((err = set_fifo_scheduling(&sender_attr, 0)) != 0) {
            fprintf(stderr,
                    "set_fifo_scheduling: Failed to set FIFO scheduling (%d)\n",
                    err);
            exit(SCHED_ERROR);
        }
    } else {
        fprintf(stderr,
                "WARNING: Failed to set FIFO scheduling, i.e. euid not \
root!\n");
    }

    if ((err = pthread_create(&sender_thread, &sender_attr, file_sender,
                              (void *)&sender_params)) < 0) {
        fprintf(stderr,
                "pthread_create: Failed to start file sender thread (%d)\n",
                err);
        exit(THREAD_ERROR);
    }

    pthread_join(sender_thread, NULL);
    return 0;
}
