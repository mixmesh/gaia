#include <stdbool.h>
#include <erl_nif.h>
#include <alsa/asoundlib.h>
#include "jb.h"
#include "jb_table.h"
#include "audio_sink.h"
#include "network_receiver.h"
#include "timing.h"
#include "globals.h"
#include "gaia_utils.h"

#define ATOM(name) atm_##name
#define DECL_ATOM(name) ERL_NIF_TERM atm_##name = 0
#define LOAD_ATOM(name)                         \
    do {                                                                \
        if (!enif_make_existing_atom(env, #name, &atm_##name, ERL_NIF_LATIN1)) \
            return -1;                                                  \
    } while (0)

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(already_started);
DECL_ATOM(not_started);
DECL_ATOM(bad_params);
DECL_ATOM(addr_port);
DECL_ATOM(opus_enabled);
DECL_ATOM(pcm_name);

#define MAX_ADDR_LEN 64
#define MAX_PCM_NAME_LEN 64

bool started = false;
pthread_rwlock_t *params_rwlock;
uint64_t params_last_updated;
ErlNifTid network_receiver_tid;
network_receiver_params_t receiver_params = {.addr_port = NULL};
ErlNifTid audio_sink_tid;
audio_sink_params_t audio_sink_params = {.pcm_name = NULL};

// Shared data (same as in gaia.c)
jb_table_t *jb_table;
bool kill_network_sender = false;
bool kill_network_receiver = false;
bool kill_audio_sink = false;
uint8_t *playback_packet;
thread_mutex_t *playback_packet_mutex;

void take_params_rdlock(void) {
    assert(pthread_rwlock_rdlock(params_rwlock) == 0);
}

void take_params_wrlock(void) {
    assert(pthread_rwlock_wrlock(params_rwlock) == 0);
}

void release_params_lock(void) {
    assert(pthread_rwlock_unlock(params_rwlock) == 0);
}

bool parse_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    const ERL_NIF_TERM *params_tuple;
    int arity;

    if (enif_get_tuple(env, argv[0], &arity, &params_tuple)) {
        if (arity != 2) {
            return false;
        }

        ErlNifMapIterator iter;

        // Type: alsa_nif:network_receiver_params()
        in_addr_t network_receiver_addr;
        unsigned int network_receiver_port;
        bool network_receiver_opus_enabled;
        if (enif_map_iterator_create(env, params_tuple[0], &iter,
                                     ERL_NIF_MAP_ITERATOR_FIRST)) {
            ERL_NIF_TERM key, value;
            while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                if (key == ATOM(addr_port)) {
                    const ERL_NIF_TERM *addr_port_tuple;
                    if (enif_get_tuple(env, value, &arity, &addr_port_tuple)) {
                        if (arity != 2) {
                            return false;
                        }
                        char addr_string[MAX_ADDR_LEN];
                        if (enif_get_string(env, addr_port_tuple[0], addr_string,
                                            MAX_ADDR_LEN, ERL_NIF_LATIN1) > 0) {
                            network_receiver_addr = inet_addr(addr_string);
                            if (!enif_get_uint(env, addr_port_tuple[1],
                                               &network_receiver_port)) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else if (key == ATOM(opus_enabled)) {
                    if (value == ATOM(true)) {
                        network_receiver_opus_enabled = true;
                    } else if (value == ATOM(false)) {
                        network_receiver_opus_enabled = false;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
                enif_map_iterator_next(env, &iter);
            }
        } else {
            return false;
        }

        // Type: alsa_nif:audio_sink_params()
        char audio_sink_pcm_name[MAX_PCM_NAME_LEN];
        bool audio_sink_opus_enabled;
        if (enif_map_iterator_create(env, params_tuple[1], &iter,
                                     ERL_NIF_MAP_ITERATOR_FIRST)) {
            ERL_NIF_TERM key, value;
            while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
                if (key == ATOM(pcm_name)) {
                    if (enif_get_string(env, value, audio_sink_pcm_name,
                                        MAX_PCM_NAME_LEN, ERL_NIF_LATIN1) < 1) {
                        return false;
                    }
                } else if (key == ATOM(opus_enabled)) {
                    if (value == ATOM(true)) {
                        audio_sink_opus_enabled = true;
                    } else if (value == ATOM(false)) {
                        audio_sink_opus_enabled = false;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
                enif_map_iterator_next(env, &iter);
            }
        } else {
            return false;
        }


        if (receiver_params.addr_port != NULL) {
            free(receiver_params.addr_port);
        }
        receiver_params.addr_port = malloc(sizeof(addr_port_t));
        receiver_params.addr_port->addr = network_receiver_addr;
        receiver_params.addr_port->port = network_receiver_port;
        receiver_params.opus_enabled = network_receiver_opus_enabled;

        if (audio_sink_params.pcm_name != NULL) {
            free(audio_sink_params.pcm_name);
        }
        audio_sink_params.pcm_name = strdup(audio_sink_pcm_name);
        audio_sink_params.opus_enabled = audio_sink_opus_enabled;

        params_last_updated = utimestamp();

        return true;
    } else {
        return false;
    }
}

/*
 * start
 */

static ERL_NIF_TERM _start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (started) {
        return enif_make_tuple2(env, ATOM(error), ATOM(already_started));
    }

    if (!parse_params(env, argc, argv)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(bad_params));
    }

    // Create locks
    params_rwlock = malloc(sizeof(pthread_rwlock_t));
    assert(pthread_rwlock_init(params_rwlock, NULL) == 0);

    // Create jitter buffer table
    jb_table = jb_table_new();

    // Create playback packet data and mutex
    playback_packet = malloc(PERIOD_SIZE_IN_BYTES);
    playback_packet_mutex = malloc(sizeof(thread_mutex_t));
    assert(thread_mutex_init(playback_packet_mutex,
                             "playback_packet_mutex") == 0);

    // Start network receiver thread
    enif_thread_create("network_receiver", &network_receiver_tid,
                       network_receiver, (void *)&receiver_params, NULL);

    // Start audio sink thread
    enif_thread_create("audio_sink", &audio_sink_tid, audio_sink,
                       (void *)&audio_sink_params, NULL);

    started = true;
    return ATOM(ok);
}

/*
 * stop
 */

static ERL_NIF_TERM _stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (!started) {
        return enif_make_tuple2(env, ATOM(error), ATOM(not_started));
    }

    // Wait for audio sink thread to die
    kill_audio_sink = true;
    DEBUGP("Waiting for audio sink to kill itself");
    enif_thread_join(audio_sink_tid, NULL);
    DEBUGP("Audio sink has killed itself");
    kill_audio_sink = false;

    // Wait for network receiver thread to die
    kill_network_receiver = true;
    DEBUGP("Waiting for network receiver to kill itself");
    enif_thread_join(network_receiver_tid, NULL);
    DEBUGP("Network receiver has killed itself");
    kill_network_receiver = false;

    // Remove locks
    assert(pthread_rwlock_destroy(params_rwlock) == 0);
    free(params_rwlock);

    // Remove jitter buffer table
    jb_table_free(jb_table, false);

    // Remove playback packet data and mutex
    assert(thread_mutex_lock(playback_packet_mutex) == 0);
    free(playback_packet);
    assert(thread_mutex_unlock(playback_packet_mutex) == 0);
    assert(thread_mutex_destroy(playback_packet_mutex) == 0);
    free(playback_packet_mutex);

    DEBUGP("All is good. We can die in peace.");
    started = false;
    return ATOM(ok);
}

/*
 * set_params
 */

static ERL_NIF_TERM _set_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    take_params_wrlock();
    if (!parse_params(env, argc, argv)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(bad_params));
    }
    release_params_lock();
    return ATOM(ok);
}

/*
 * set_params
 */

static ERL_NIF_TERM _read_packet(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    assert(thread_mutex_lock(playback_packet_mutex) == 0);
    ERL_NIF_TERM bin;
    uint8_t *data =
        (uint8_t *)enif_make_new_binary(env, PERIOD_SIZE_IN_BYTES, &bin);
    memcpy(data, playback_packet, PERIOD_SIZE_IN_BYTES);
    assert(thread_mutex_unlock(playback_packet_mutex) == 0);
    return bin;
}

/*
 * load
 */

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(true);
    LOAD_ATOM(false);
    LOAD_ATOM(already_started);
    LOAD_ATOM(not_started);
    LOAD_ATOM(bad_params);
    LOAD_ATOM(addr_port);
    LOAD_ATOM(opus_enabled);
    LOAD_ATOM(pcm_name);
    return 0;
}

/*
 * unload
 */

static void unload(ErlNifEnv* env, void* priv_data) {
}

/*
 * NIF functions registration
 */

static ErlNifFunc nif_funcs[] =
    {
     {"start", 1, _start, 0},
     {"stop", 0, _stop, ERL_NIF_DIRTY_JOB_IO_BOUND},
     {"set_params", 1, _set_params, 0},
     {"read_packet", 1, _read_packet, ERL_NIF_DIRTY_JOB_IO_BOUND}
    };

ERL_NIF_INIT(gaia_nif, nif_funcs, load, NULL, NULL, unload);
