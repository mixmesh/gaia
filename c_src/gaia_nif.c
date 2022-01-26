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
#include "source_table.h"

#define ATOM(name) atm_##name
#define DECL_ATOM(name) ERL_NIF_TERM atm_##name = 0
#define LOAD_ATOM(name)                         \
    do { \
        if (!enif_make_existing_atom(env, #name, &atm_##name, ERL_NIF_LATIN1)) \
            return -1; \
    } while (0)

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(already_started);
DECL_ATOM(not_started);
DECL_ATOM(bad_params);
DECL_ATOM(port);
DECL_ATOM(playback_audio);
DECL_ATOM(pcm_name);
DECL_ATOM(peer);
DECL_ATOM(group);

#define MAX_ADDR_LEN 64
#define MAX_PCM_NAME_LEN 64
#define MAX_SRC_ADDRS 256

bool started = false;
pthread_rwlock_t *params_rwlock;
uint64_t params_last_updated;
ErlNifTid network_receiver_tid;
ErlNifTid audio_sink_tid;
audio_sink_params_t audio_sink_params = {.pcm_name = NULL};

// Shared data (same as in gaia.c)
jb_table_t *jb_table;
bool kill_network_sender = false;
bool kill_network_receiver = false;
bool kill_audio_sink = false;
uint8_t *playback_packet;
thread_mutex_t *playback_packet_mutex;
source_table_t *source_table;

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
    // Type: gaia_nif:audio_sink_params()
    ErlNifMapIterator iter;
    char audio_sink_pcm_name[MAX_PCM_NAME_LEN];
    bool audio_sink_playback_audio = true;
    if (enif_map_iterator_create(env, argv[0], &iter,
                                 ERL_NIF_MAP_ITERATOR_FIRST)) {
        ERL_NIF_TERM key, value;
        while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
            if (key == ATOM(pcm_name)) {
                if (enif_get_string(env, value, audio_sink_pcm_name,
                                    MAX_PCM_NAME_LEN, ERL_NIF_LATIN1) < 1) {
                    return false;
                }
            } else if (key == ATOM(playback_audio)) {
                if (value == ATOM(true)) {
                    audio_sink_playback_audio = true;
                } else if (value == ATOM(false)) {
                    audio_sink_playback_audio = false;
                } else {
                    return false;
                }
            } else {
                return false;
            }
            enif_map_iterator_next(env, &iter);
        }

        if (audio_sink_params.pcm_name != NULL) {
            free(audio_sink_params.pcm_name);
        }

        audio_sink_params.pcm_name = strdup(audio_sink_pcm_name);
        audio_sink_params.playback_audio = audio_sink_playback_audio;

        params_last_updated = utimestamp();

        return true;
    } else {
        return false;
    }

}

/*
 * start
 */

static ERL_NIF_TERM _start(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[]) {
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

    // Create source table
    source_table = source_table_new();

    // Create playback packet data and mutex
    playback_packet = malloc(PERIOD_SIZE_IN_BYTES);
    playback_packet_mutex = malloc(sizeof(thread_mutex_t));
    assert(thread_mutex_init(playback_packet_mutex,
                             "playback_packet_mutex") == 0);

    // Start network receiver thread
    enif_thread_create("network_receiver", &network_receiver_tid,
                       network_receiver, NULL, NULL);

    // Start audio sink thread
    enif_thread_create("audio_sink", &audio_sink_tid, audio_sink,
                       (void *)&audio_sink_params, NULL);

    started = true;
    return ATOM(ok);
}

/*
 * stop
 */

static ERL_NIF_TERM _stop(ErlNifEnv* env, int argc,
                          const ERL_NIF_TERM argv[]) {
    if (!started) {
        return enif_make_tuple2(env, ATOM(error), ATOM(not_started));
    }

    // Wait for audio sink thread to die
    kill_audio_sink = true;
    INFOF("Waiting for audio sink to kill itself");
    enif_thread_join(audio_sink_tid, NULL);
    INFOF("Audio sink has killed itself");
    kill_audio_sink = false;

    // Wait for network receiver thread to die
    kill_network_receiver = true;
    INFOF("Waiting for network receiver to kill itself");
    enif_thread_join(network_receiver_tid, NULL);
    INFOF("Network receiver has killed itself");
    kill_network_receiver = false;

    // Remove locks
    assert(pthread_rwlock_destroy(params_rwlock) == 0);
    free(params_rwlock);

    // Remove jitter buffer table
    jb_table_free(jb_table, false);

    // Remove source table
    source_table_free(source_table);

    // Remove playback packet data and mutex
    assert(thread_mutex_lock(playback_packet_mutex) == 0);
    free(playback_packet);
    assert(thread_mutex_unlock(playback_packet_mutex) == 0);
    assert(thread_mutex_destroy(playback_packet_mutex) == 0);
    free(playback_packet_mutex);

    INFOF("All is good. We can die in peace.");
    started = false;
    return ATOM(ok);
}

/*
 * set_params
 */

static ERL_NIF_TERM _set_params(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
    take_params_wrlock();
    if (!parse_params(env, argc, argv)) {
        return enif_make_tuple2(env, ATOM(error), ATOM(bad_params));
    }
    release_params_lock();
    return ATOM(ok);
}

/*
 * read_packet
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
 * set_sources
 */

static ERL_NIF_TERM _set_sources(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    if (enif_is_list(env, argv[0])) {
        source_table_take_mutex(source_table);

        // Mark all sources as not used
        void mark_source_as_unused(source_t *source) {
            source->used = false;
        }
        source_table_foreach(source_table, mark_source_as_unused);

        // Generate [{{peer | group, gaia_serv:id()}, LocalPort :: integer()}]
        ERL_NIF_TERM item, items = argv[0];
        ERL_NIF_TERM return_value = enif_make_list(env, 0);
        while(enif_get_list_cell(env, items, &item, &items)) {
            const ERL_NIF_TERM *tuple;
            int arity;
            if (enif_get_tuple(env, item, &arity, &tuple)) {
                if (!((arity == 2 && tuple[0] == ATOM(peer)) ||
                      (arity == 3 && tuple[0] == ATOM(group)))) {
                    source_table_release_mutex(source_table);
                    return enif_make_badarg(env);
                }
                unsigned int id;
                if (!enif_get_uint(env, tuple[1], &id)) {
                    source_table_release_mutex(source_table);
                    return enif_make_badarg(env);
                }
                source_t *source = source_table_find(source_table, id);
                if (source == NULL) {
                    unsigned int local_port;
                    if (arity == 2) { // peer
                        local_port = 0;
                    } else { // group
                        if (!enif_get_uint(env, tuple[2], &local_port)) {
                            source_table_release_mutex(source_table);
                            return enif_make_badarg(env);
                        }
                    }
                    // Create and bind new UDP socket
                    int sockfd;
                    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
                        perror("socket: Socket creation failed");
                        source_table_release_mutex(source_table);
                        return enif_make_badarg(env);
                    }
                    struct sockaddr_in addr;
                    memset(&addr, 0, sizeof(addr));
                    addr.sin_family = AF_INET;
                    addr.sin_addr.s_addr = htonl(INADDR_ANY);
                    addr.sin_port = local_port;
                    if (bind(sockfd, (struct sockaddr *)&addr,
                             sizeof(addr)) < 0) {
                        perror("bind: Binding of socket failed");
                        close(sockfd);
                        source_table_release_mutex(source_table);
                        return enif_make_badarg(env);
                    }
                    if (arity == 2) { // peer
                        // Figure out allocated port number
                        struct sockaddr_in local_addr;
                        socklen_t local_addrlen = sizeof(local_addr);
                        if (getsockname(sockfd, (struct sockaddr*)&local_addr,
                                        &local_addrlen) == -1) {
                            perror("getsockname: No port allocated for socket");
                            close(sockfd);
                            source_table_release_mutex(source_table);
                            return enif_make_badarg(env);
                        }
                        local_port = (int)ntohs(local_addr.sin_port);
                    }
                    // Add the source to the source table
                    source_t *new_source = source_new();
                    new_source->id = id;
                    new_source->sockfd = sockfd;
                    new_source->port = local_port;
                    new_source->used = true;
                    source_table_add(source_table, new_source);
                    INFOF("Set source: %d (%d)", new_source->id,
                          new_source->port);

                    // Add local port to return value list
                    ERL_NIF_TERM id_tuple;
                    if (arity == 2) {
                        id_tuple = enif_make_tuple2(env, ATOM(peer),
                                                    enif_make_int(env, id));
                    } else {
                        id_tuple = enif_make_tuple2(env, ATOM(group),
                                                    enif_make_int(env, id));
                    }
                    ERL_NIF_TERM source_tuple =
                        enif_make_tuple2(env, id_tuple,
                                         enif_make_int(env, local_port));
                    return_value =
                        enif_make_list_cell(env, source_tuple, return_value);
                } else {
                    source->used = true;
                }
            }
        }

        // Delete unused sources
        void delete_unused_source(source_t *source) {
            if (!source->used) {
                source_table_delete(source_table, source);
            }
        }
        source_table_foreach(source_table, delete_unused_source);
        source_table_release_mutex(source_table);

        return return_value;
    } else {
        source_table_release_mutex(source_table);
        return enif_make_badarg(env);
    }
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
    LOAD_ATOM(port);
    LOAD_ATOM(playback_audio);
    LOAD_ATOM(pcm_name);
    LOAD_ATOM(peer);
    LOAD_ATOM(group);
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
     {"read_packet", 0, _read_packet, ERL_NIF_DIRTY_JOB_IO_BOUND},
     {"set_sources", 1, _set_sources, 0}
    };

ERL_NIF_INIT(gaia_nif, nif_funcs, load, NULL, NULL, unload);
