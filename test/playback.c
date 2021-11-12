#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include "../audio.h"
#include "../globals.h"

// $ ./capture test.dat
// $ ./playback test.dat

#define MAX_SAMPLES 3

audio_info_t *audio_info = NULL;
FILE *fds[MAX_SAMPLES];
uint8_t nfds = 0;

void stop() {
    for (uint8_t i = 0; i < nfds; i++) {
        fclose(fds[i]);
    }
    if (audio_info != NULL) {
        audio_free(audio_info);
    }
    exit(0);
}

int main(int argc, char *argv[]) {
    if (argc < 2 || argc > 4) {
        exit(ARG_ERROR);
    }

    nfds = argc - 1;

    if (signal(SIGINT, stop) == SIG_ERR) {
        perror("signal");
        exit(INTERNAL_ERROR);
    }

    int err;
    if ((err = audio_new(PCM_NAME, SND_PCM_STREAM_PLAYBACK, 0, FORMAT,
                         CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                         PERIOD_SIZE_IN_FRAMES, BUFFER_MULTIPLICATOR,
                         &audio_info)) < 0) {
        fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
                snd_strerror(err));
        exit(AUDIO_ERROR);
    }
    audio_print_parameters(audio_info, "playback");
    assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);

    for (uint8_t i = 0; i < nfds; i++) {
        if ((fds[i] = fopen(argv[i + 1], "r")) == NULL) {
            perror(argv[i + 1]);
            exit(FILE_ERROR);
        }
    }

    uint8_t bufs[MAX_SAMPLES][PERIOD_SIZE_IN_BYTES];

    while (true) {
        uint8_t nbufs = 0;
        for (uint8_t i = 0; i < nfds; i++) {
            if (fread(bufs[i], 1, PERIOD_SIZE_IN_BYTES, fds[i]) ==
                PERIOD_SIZE_IN_BYTES) {
                nbufs++;
            }
        }

        uint8_t *buf;
        if (nbufs == 0) {
            break;
        } else if (nbufs == 1) {
            buf = bufs[0];
        } else {
            uint16_t *data[MAX_SAMPLES], mixed_data[PERIOD_SIZE_IN_BYTES];
            for (uint8_t i; i < nbufs; i++) {
                data[i] = (uint16_t *)bufs[i];
            }
            assert(audio_umix16(data, nbufs, mixed_data) == 0);
            buf = (uint8_t *)mixed_data;
        }

        if (audio_write(audio_info, buf, PERIOD_SIZE_IN_FRAMES) < 0) {
            break;
        }
    }

    stop();
}
