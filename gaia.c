#include <unistd.h>
#include <stdio.h>
#include "network_sender.h"
#include "network_receiver.h"
#include "audio_sink.h"
#include "jb_table.h"
#include "timing.h"
#include "globals.h"
#include "gaia_utils.h"

#define MAX_NETWORK_SENDER_ADDR_PORTS 256

jb_table_t *jb_table;

void usage(char *argv[]) {
    fprintf(stderr,
            "Usage: %s [-D device] [-s addr[:port]] [-d addr[:port]] userid\n",
            argv[0]);
    fprintf(stderr, "Note: device, addr and port respectively defaults to %s, \
%s and %d\n", DEFAULT_PCM_NAME, DEFAULT_ADDR, DEFAULT_PORT);
    fprintf(stderr,
            "Example: sudo %s -D plughw:0.0 -s 172.16.0.116:2305 -d 172.16.0.95 -d \
172.16.0.95:2356 4711\n",
            argv[0]);
    exit(ARG_ERROR);
}

int main (int argc, char *argv[]) {
    int err;
    in_addr_t src_addr = inet_addr(DEFAULT_ADDR);
    uint16_t src_port = DEFAULT_PORT;
    char* pcm_name = DEFAULT_PCM_NAME;

    network_sender_addr_port_t dest_addr_ports[MAX_NETWORK_SENDER_ADDR_PORTS];
    dest_addr_ports[0].addr = inet_addr(DEFAULT_ADDR);
    dest_addr_ports[0].port = DEFAULT_PORT;

    int opt, ndest_addr_ports = 0;

    while ((opt = getopt(argc, argv, "s:d:D:")) != -1) {
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
            ndest_addr_ports =
                (ndest_addr_ports + 1) % MAX_NETWORK_SENDER_ADDR_PORTS;
            break;
        case 'D':
            pcm_name = strdup(optarg);
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
         .pcm_name = pcm_name,
         .userid = userid,
         .naddr_ports = ndest_addr_ports,
         .addr_ports = dest_addr_ports,
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

    if ((err = pthread_create(&sender_thread, &sender_attr, network_sender,
                              (void *)&sender_params)) < 0) {
        fprintf(stderr, "pthread_create: Failed to start sender thread (%d)\n",
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
    if ((err = pthread_attr_init(&receiver_attr)) != 0) {
        fprintf(stderr,
                "pthread_attr_init: Failed to initialize receiver thread attribute \
(%d)\n",
                err);
        exit(THREAD_ERROR);
    }

    if (geteuid() == 0) {
        if ((err = set_fifo_scheduling(&receiver_attr, 0)) != 0) {
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

    if ((err = pthread_create(&receiver_thread, &receiver_attr, network_receiver,
                              (void *)&receiver_params)) < 0) {
        fprintf(stderr, "pthread_create: Failed to start receiver thread \
(%d)\n",
                err);
        exit(THREAD_ERROR);
    }

    msleep(500);

    // Start audio sink thread
    pthread_t audio_sink_thread;
    audio_sink_params_t audio_sink_params =
        {
         .pcm_name = pcm_name
        };

    pthread_attr_t audio_sink_attr;
    if ((err = pthread_attr_init(&audio_sink_attr)) != 0) {
        fprintf(stderr,
                "pthread_attr_init: Failed to initialize audio sink thread \
attribute (%d)\n",
                err);
        exit(THREAD_ERROR);
    }

    if (geteuid() == 0) {
        if ((err = set_fifo_scheduling(&audio_sink_attr, 0)) != 0) {
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

    if ((err = pthread_create(&audio_sink_thread, &audio_sink_attr, audio_sink,
                              (void *)&audio_sink_params)) < 0) {
        fprintf(stderr,
                "pthread_create: Failed to start network audio sink thread \
(%d)\n",
                err);
        exit(THREAD_ERROR);
    }

    pthread_join(sender_thread, NULL);
    pthread_join(receiver_thread, NULL);
    pthread_join(audio_sink_thread, NULL);
    jb_table_free(jb_table, false);
    return 0;
}
