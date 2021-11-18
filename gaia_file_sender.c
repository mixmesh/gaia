#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <string.h>
#include "file_sender.h"
#include "gaia_utils.h"
#include "globals.h"

void usage(char *argv[]) {
    fprintf(stderr, "Usage: %s [-d addr[:port]] [-u userid] filename ...\n",
            argv[0]);
    fprintf(stderr,
            "Note: addr and port default to %s and %d\n", DEFAULT_ADDR,
            DEFAULT_PORT);
    fprintf(stderr,
            "Example: sudo %s -d 172.16.0.95 -d 172.16.0.95:2356 -u 1000 \
foo.u16\n",
            argv[0]);
    exit(ARG_ERROR);
}

int main (int argc, char *argv[]) {
    int err;
    char *pcm_name = DEFAULT_PCM_NAME;

    file_sender_addr_port_t addr_ports[MAX_USERS];
    addr_ports[0].addr = inet_addr(DEFAULT_ADDR);
    addr_ports[0].port = DEFAULT_PORT;

    int opt, naddr_ports = 0;
    uint32_t userid = 1;
    long value;

    while ((opt = getopt(argc, argv, "u:d:")) != -1) {
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
        case 'D':
            pcm_name = strdup(optarg);
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
        params[i]->pcm_name = pcm_name;
        params[i]->userid = userid++;
        memcpy(params[i]->filename, filename, strlen(filename) + 1);
        params[i]->naddr_ports = naddr_ports;
        params[i]->addr_ports = addr_ports;

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
