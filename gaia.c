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
            "\
Usage: %s [-D device] [-E device] [-s addr[:port]] [-d addr[:port] -d ...] [-x] userid\n\
\n\
Example: \n\
  sudo %s -D plughw:1,0 -d 172.16.0.95 -d 172.16.0.95:2356 -s 172.16.0.116:2305 1000\n\
\n\
Options:\n\
  -D Use this device to capture audio (%s)\n\
  -E Use this device to playback audio (%s)\n\
  -d Send audio streams to this destination address and port (%s:%d)\n\
  -s Bind to this source address and port (%s:%d)\n\
  -x Enable use of Opus audio codec\n",
            argv[0], argv[0], DEFAULT_PCM_NAME, DEFAULT_PCM_NAME,
            DEFAULT_ADDR, DEFAULT_PORT, DEFAULT_ADDR, DEFAULT_PORT);
    exit(ARG_ERROR);
}

int main (int argc, char *argv[]) {
    int err;

    char *capture_pcm_name = DEFAULT_PCM_NAME;
    char *playback_pcm_name = DEFAULT_PCM_NAME;

    addr_port_t dest_addr_ports[MAX_NETWORK_SENDER_ADDR_PORTS];
    dest_addr_ports[0].addr = inet_addr(DEFAULT_ADDR);
    dest_addr_ports[0].port = DEFAULT_PORT;
    int ndest_addr_ports = 0;

    addr_port_t src_addr_port =
        {
         .addr = inet_addr(DEFAULT_ADDR),
         .port = DEFAULT_PORT
        };

    bool opus_enabled = false;

    int opt;

    while ((opt = getopt(argc, argv, "D:E:d:s:x")) != -1) {
        switch (opt) {
        case 'D':
            capture_pcm_name = strdup(optarg);
            break;
        case 'E':
            playback_pcm_name = strdup(optarg);
            break;
        case 'd':
            if (get_addr_port(optarg, &dest_addr_ports[ndest_addr_ports].addr,
                              &dest_addr_ports[ndest_addr_ports].port) < 0) {
                usage(argv);
            }
            ndest_addr_ports =
                (ndest_addr_ports + 1) % MAX_NETWORK_SENDER_ADDR_PORTS;
            break;
        case 's':
            if (get_addr_port(optarg, &src_addr_port.addr,
                              &src_addr_port.port) < 0) {
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
         .pcm_name = capture_pcm_name,
         .userid = userid,
         .naddr_ports = ndest_addr_ports,
         .addr_ports = dest_addr_ports,
         .opus_enabled = opus_enabled
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
         .addr_port = &src_addr_port,
         .opus_enabled = opus_enabled
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
         .pcm_name = playback_pcm_name,
         .opus_enabled = opus_enabled
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
