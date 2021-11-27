#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <string.h>
#include "file_sender.h"
#include "globals.h"

void usage(char *argv[]) {
    fprintf(stderr,
            "\
Usage: %s [-d addr[:port] -d ...] [-u userid] filename ...\n\
\n\
Example:\n\
  sudo %s -d 172.16.0.95 -d 172.16.0.95:2356 -u 1000\n\
\n\
Options:\n\
  -d Send audio streams to this destination address and port (%s:%d)\n\
  -u Use this userid as a base, i.e. will be increased one step for each -d option (1)\n\
  -x Enable use of Opus audio codec\n",
            argv[0], argv[0], DEFAULT_ADDR, DEFAULT_PORT);
    exit(ARG_ERROR);
}

int main (int argc, char *argv[]) {
    int err;

    addr_port_t addr_ports[MAX_USERS];
    addr_ports[0].addr = inet_addr(DEFAULT_ADDR);
    addr_ports[0].port = DEFAULT_PORT;
    int naddr_ports = 0;

    uint32_t userid = 1;

    bool opus_enabled = false;

    int opt;
    long value;

    while ((opt = getopt(argc, argv, "u:d:x")) != -1) {
        switch (opt) {
        case 'd':
            if (get_addr_port(optarg, &addr_ports[naddr_ports].addr,
                              &addr_ports[naddr_ports].port) < 0) {
                usage(argv);
            }
            naddr_ports = (naddr_ports + 1) % MAX_USERS;
            break;
        case 'u':
            if (string_to_long(optarg, &value) < 0) {
                usage(argv);
            }
            userid = value;
            if (userid == 0) {
                usage(argv);
            }
            break;
        case 'x':
            opus_enabled = true;
            break;
        default:
            usage(argv);
        }
    }

    if (naddr_ports == 0) {
        naddr_ports = 1;
    }

    int nfilenames = argc - optind;
    if (nfilenames < 1) {
        usage(argv);
    }

    pthread_t sender_threads[MAX_USERS];
    file_sender_params_t *params[MAX_USERS];
    uint8_t nsender_threads = 0;

    // Start sender threads
    for (int i = 0; i < nfilenames; i++) {
        char *filename = argv[i + optind];
        printf("Preparing to send: %s\n", filename);

        // Check file
        struct stat buffer;
        if (stat(filename, &buffer) != 0) {
            fprintf(stderr, "%s does not exist\n", filename);
            break;
        }

        params[i] = malloc(sizeof(file_sender_params_t));
        params[i]->userid = userid++;
        memcpy(params[i]->filename, filename, strlen(filename) + 1);
        params[i]->naddr_ports = naddr_ports;
        params[i]->addr_ports = addr_ports;
        params[i]->opus_enabled = opus_enabled;

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
                        "set_fifo_scheduling: Failed to set FIFO scheduling \
(%d)\n",
                        err);
                exit(SCHED_ERROR);
            }
        } else {
            fprintf(stderr,
                    "WARNING: Failed to set FIFO scheduling, i.e. euid not \
root!\n");
        }

        if ((err = pthread_create(&sender_threads[i], &sender_attr, file_sender,
                                  (void *)params[i])) < 0) {
            fprintf(stderr,
                    "pthread_create: Failed to start file sender thread (%d)\n",
                    err);
            exit(THREAD_ERROR);
        }

        nsender_threads++;
    }

    for (int i = 0; i < nsender_threads; i++) {
        pthread_join(sender_threads[i], NULL);
        free(&params[i]);
    }

    return 0;
}
