#include <string.h>
#include <stdio.h>
#include <erl_nif.h>
#include <opus/opus.h>

// Read: https://wiki.xiph.org/OpusFAQ
// Read: https://wiki.xiph.org/Opus_Recommended_Settings

#define ATOM(name) atm_##name
#define DECL_ATOM(name) ERL_NIF_TERM atm_##name = 0
#define LOAD_ATOM(name) \
    do { \
        if (!enif_make_existing_atom(env, #name, &atm_##name, ERL_NIF_LATIN1)) \
            return -1; \
    } while (0)

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(voip);
DECL_ATOM(audio);
DECL_ATOM(restricted_lowdelay);

ErlNifResourceType *encoder_resource_type;

/*
 * create_encoder
 */

static ERL_NIF_TERM _create_encoder(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
    int rate_in_hz;
    if (!enif_get_int(env, argv[0], &rate_in_hz)) {
        return enif_make_badarg(env);
    }
    int channels;
    if (!enif_get_int(env, argv[1], &channels)) {
        return enif_make_badarg(env);
    }
    int application;
    if (argv[2] == ATOM(voip)) {
        application = OPUS_APPLICATION_VOIP;
    } else if (argv[2] == ATOM(audio)) {
        application = OPUS_APPLICATION_AUDIO;
    } else if (argv[2] == ATOM(restricted_lowdelay)) {
        application = OPUS_APPLICATION_RESTRICTED_LOWDELAY;
    } else {
        return enif_make_badarg(env);
    }
    int complexity;
    if (!enif_get_int(env, argv[1], &complexity)) {
        return enif_make_badarg(env);
    }

    OpusEncoder *encoder =
        enif_alloc_resource(encoder_resource_type,
                            opus_encoder_get_size(channels));
    int err;
    if ((err = opus_encoder_init(encoder, (opus_int32)rate_in_hz, channels,
                                 application)) == OPUS_OK) {
        opus_encoder_ctl(encoder, OPUS_SET_COMPLEXITY(complexity));
        ERL_NIF_TERM encoder_resource = enif_make_resource(env, encoder);
        enif_release_resource(encoder);
        return enif_make_tuple2(env, ATOM(ok), encoder_resource);
    } else {
        return enif_make_tuple2(env, ATOM(error), enif_make_int(env, err));
    }
}

/*
 * encode
 */

static ERL_NIF_TERM _encode(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]) {
    OpusEncoder *encoder;
    if (!enif_get_resource(env, argv[0], encoder_resource_type,
                           (void **)&encoder)) {
        return enif_make_badarg(env);
    }
    int max_packet_size;
    if (!enif_get_int(env, argv[1], &max_packet_size)) {
        return enif_make_badarg(env);
    }
    int channels;
    if (!enif_get_int(env, argv[2], &channels)) {
        return enif_make_badarg(env);
    }
    int sample_size_in_bytes;
    if (!enif_get_int(env, argv[3], &sample_size_in_bytes)) {
        return enif_make_badarg(env);
    }
    ErlNifBinary pcm_bin;
    if (enif_inspect_binary(env, argv[4], &pcm_bin)) {
        uint8_t opus_buf[max_packet_size];
        int packet_len;
        int period_size_in_frames =
            pcm_bin.size / channels / sample_size_in_bytes;
        if ((packet_len = opus_encode(encoder, (opus_int16 *)pcm_bin.data,
                                      period_size_in_frames, opus_buf,
                                      max_packet_size)) < 0) {
            return enif_make_tuple2(env, ATOM(error),
                                    enif_make_int(env, packet_len));
        } else {
            ERL_NIF_TERM bin;
            uint8_t *data =
                (uint8_t *)enif_make_new_binary(env, packet_len, &bin);
            memcpy(data, opus_buf, packet_len);
            return enif_make_tuple2(env, ATOM(ok), bin);
        }
    } else {
        return enif_make_badarg(env);
    }
}

/*
 * load
 */

void encoder_resource_dtor(ErlNifEnv *env, void *obj) {
    OpusEncoder *encoder = (OpusEncoder *)obj;
    opus_encoder_destroy(encoder);
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(voip);
    LOAD_ATOM(audio);
    LOAD_ATOM(restricted_lowdelay);

    if ((encoder_resource_type =
         enif_open_resource_type(env, NULL, "opus_nif", encoder_resource_dtor,
                                 ERL_NIF_RT_CREATE, NULL)) == NULL) {
        return -1;
    }

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
     {"create_encoder", 4, _create_encoder, 0},
     {"encode", 5, _encode, 0},
    };

ERL_NIF_INIT(opus, nif_funcs, load, NULL, NULL, unload);
