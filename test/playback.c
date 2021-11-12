#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include "../audio.h"
#include "../globals.h"

// $ ./capture test.dat
// $ ./playback test.dat

#define MAX_FILES 128

typedef struct {
    FILE *fd;
    bool activated;
} file_t;

file_t files[MAX_FILES];
uint8_t nfiles = 0;

audio_info_t *audio_info = NULL;

void stop() {
    for (uint8_t i = 0; i < nfiles; i++) {
        fclose(files[i].fd);
    }
    if (audio_info != NULL) {
        audio_free(audio_info);
    }
    exit(0);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        exit(ARG_ERROR);
    }

    nfiles = argc - 1;

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

    for (uint8_t i = 0; i < nfiles; i++) {
        if ((files[i].fd = fopen(argv[i + 1], "r")) == NULL) {
            perror(argv[i + 1]);
            exit(FILE_ERROR);
        }
        files[i].activated = true;
    }

    uint16_t *data[nfiles];

    while (true) {
        uint8_t nactive = 0;
        for (uint8_t i = 0; i < nfiles; i++) {
            if (!files[i].activated) {
                continue;
            }
            if (fread(data[nactive], 1, PERIOD_SIZE_IN_BYTES, files[i].fd) !=
                PERIOD_SIZE_IN_BYTES) {
                files[i].activated = false;
            } else {
                nactive++;
            }
        }

        uint8_t *buf;
        uint16_t mixed_data[PERIOD_SIZE_IN_BYTES];
        if (nactive == 0) {
            break;
        } else if (nactive == 1) {
            buf = (uint8_t *)data[0];
        } else {
            assert(audio_umix16(data, nactive, mixed_data) == 0);
            buf = (uint8_t *)mixed_data;
        }

        if (audio_write(audio_info, buf, PERIOD_SIZE_IN_FRAMES) < 0) {
            break;
        }
    }

    stop();
}
