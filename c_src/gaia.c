#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include "network_sender.h"
#include "network_receiver.h"
#include "audio_sink.h"
#include "jb_table.h"
#include "timing.h"
#include "globals.h"
#include "gaia_utils.h"

#define MAX_NETWORK_SENDER_ADDR_PORTS 256

// Shared data (same as in gaia_nif.c)
jb_table_t *jb_table;
bool kill_network_sender = false;
bool kill_network_receiver = false;
bool kill_audio_sink = false;
uint8_t *playback_packet;
thread_mutex_t *playback_packet_mutex;

void usage(char *argv[]) {
    fprintf(stderr,
            "\
Usage: %s [-D device] [-E device] [-L] [-s addr[:port]] [-d addr[:port] -d ...] [-x] gaia-id\n\
\n\
Example: \n\
  sudo %s -D hw:1,0 -d 172.16.0.95 -d 172.16.0.95:2356 -s 172.16.0.116:2305 1000\n\
\n\
Options:\n\
  -D Use this device to capture audio (%s)\n\
  -E Use this device to playback audio (%s)\n\
  -L Do not start network sender thread\n\
  -M Do not start network receiver thread\n\
  -N Do not start audio sink thread\n\
  -d Send audio streams to this destination address and port (%s:%d)\n\
  -s Bind to this source address and port (%s:%d)\n\
  -x Enable Opus audio codec\n",
            argv[0], argv[0], DEFAULT_PCM_NAME, DEFAULT_PCM_NAME,
            DEFAULT_ADDR, DEFAULT_PORT, DEFAULT_ADDR, DEFAULT_PORT);
    exit(ARG_ERROR);
}

int start_thread(void *(*start_routine)(void *), void *restrict arg,
                 pthread_t *thread) {
    int err;
    pthread_attr_t attr;
    if ((err = pthread_attr_init(&attr)) != 0) {
        fprintf(stderr,
                "pthread_attr_init: Failed to initialize thread attribute \
(%d)\n", err);
        return THREAD_ERROR;
    }

    if (geteuid() == 0) {
        if ((err = set_fifo_scheduling(&attr, 0)) != 0) {
            fprintf(stderr, "set_fifo_scheduling: Failed to set FIFO \
scheduling (%d)\n",  err);
            return SCHED_ERROR;
        }
    } else {
        fprintf(stderr, "WARNING: Failed to set FIFO scheduling, i.e. euid not \
root!\n");
    }

    if ((err = pthread_create(thread, &attr, start_routine, arg)) < 0) {
        fprintf(stderr, "pthread_create: Failed to start sender thread (%d)\n",
                err);
        return THREAD_ERROR;
    }
    return 0;
}

int main (int argc, char *argv[]) {
    int err;

    char *capture_pcm_name = strdup(DEFAULT_PCM_NAME);
    char *playback_pcm_name = strdup(DEFAULT_PCM_NAME);

    addr_port_t dest_addr_ports[MAX_NETWORK_SENDER_ADDR_PORTS];
    dest_addr_ports[0].addr = inet_addr(DEFAULT_ADDR);
    dest_addr_ports[0].port = DEFAULT_PORT;
    int ndest_addr_ports = 0;

    addr_port_t src_addr_port =
        {
         .addr = inet_addr(DEFAULT_ADDR),
         .port = DEFAULT_PORT
        };

    bool disable_network_sender = false;
    bool disable_network_receiver = false;
    bool disable_audio_sink = false;
    bool opus_enabled = false;

    int opt;

    while ((opt = getopt(argc, argv, "D:E:LMNd:s:x")) != -1) {
        switch (opt) {
        case 'D':
            free(capture_pcm_name);
            capture_pcm_name = strdup(optarg);
            break;
        case 'E':
            free(playback_pcm_name);
            playback_pcm_name = strdup(optarg);
            break;
        case 'L':
            disable_network_sender = true;
            break;
        case 'M':
            disable_network_receiver = true;
            break;
        case 'N':
            disable_audio_sink = true;
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

    uint32_t gaia_id;
    long value;
    if (string_to_long(argv[optind], &value) < 0) {
        usage(argv);
    }
    gaia_id = value;

    if (gaia_id == 0) {
        usage(argv);
    }

    // Create jitter buffer table
    jb_table = jb_table_new();

    // Create playback packet data and mutex
    playback_packet = malloc(PERIOD_SIZE_IN_BYTES);
    playback_packet_mutex = malloc(sizeof(thread_mutex_t));
    assert(thread_mutex_init(playback_packet_mutex,
                             "playback_packet_mutex") == 0);

    // Start sender thread
    pthread_t sender_thread;
    network_sender_params_t *sender_params;
    if (!disable_network_sender) {
        sender_params = malloc(sizeof(network_sender_params_t));
        sender_params->pcm_name = capture_pcm_name;
        sender_params->gaia_id = gaia_id;
        sender_params->naddr_ports = ndest_addr_ports;
        sender_params->addr_ports = dest_addr_ports;
        sender_params->opus_enabled = opus_enabled;
        if ((err = start_thread(network_sender, (void *)sender_params,
                                &sender_thread)) != 0) {
            exit(err);
        }
    }

    // Start receiver thread
    pthread_t receiver_thread;
    network_receiver_params_t *receiver_params;
    if (!disable_network_receiver) {
        receiver_params = malloc(sizeof(network_receiver_params_t));
        receiver_params->addr_port = &src_addr_port;
        receiver_params->opus_enabled = opus_enabled;
        if ((err = start_thread(network_receiver, (void *)receiver_params,
                                &receiver_thread)) != 0) {
            exit(err);
        }
    }

    msleep(500);

    // Start audio sink thread
    pthread_t audio_sink_thread;
    audio_sink_params_t *audio_sink_params;
    if (!disable_audio_sink) {
        audio_sink_params = malloc(sizeof(audio_sink_params_t));
        audio_sink_params->pcm_name = playback_pcm_name;
        audio_sink_params->opus_enabled = opus_enabled;
        if ((err = start_thread(audio_sink, (void *)audio_sink_params,
                                &audio_sink_thread)) != 0) {
            exit(err);
        }
    }

    if (!disable_network_sender) {
        pthread_join(sender_thread, NULL);
        free(sender_params);
    }
    if (!disable_network_receiver) {
        pthread_join(receiver_thread, NULL);
        free(receiver_params);
    }
    if (!disable_audio_sink) {
        pthread_join(audio_sink_thread, NULL);
        free(audio_sink_params);
    }

    // Remove jitter buffer table
    jb_table_free(jb_table, false);

    // Remove playback packet data and mutex
    assert(thread_mutex_lock(playback_packet_mutex) == 0);
    free(playback_packet);
    assert(thread_mutex_unlock(playback_packet_mutex) == 0);
    assert(thread_mutex_destroy(playback_packet_mutex) == 0);
    free(playback_packet_mutex);

    fprintf(stderr, "All is good. We can die in peace.");
    return 0;
}
