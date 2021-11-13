#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include "../audio.h"
#include "../globals.h"

// $ ./capture test.dat
// $ ./playback test.dat

#define MAX_FILES 128
#define PEAK_PERIOD_IN_MS 400

typedef struct {
    FILE *fd;
    bool activated;
    uint8_t *data;
    uint16_t *peak_values;
    uint16_t peak_index;
} file_t;

file_t files[MAX_FILES];
uint8_t nfiles = 0;

audio_info_t *audio_info = NULL;

void stop() {
    for (uint8_t i = 0; i < nfiles; i++) {
        fclose(files[i].fd);
        free(files[i].data);
        free(files[i].peak_values);
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

    uint16_t npeak_values = PEAK_PERIOD_IN_MS / PERIOD_SIZE_IN_MS;

    for (uint8_t i = 0; i < nfiles; i++) {
        if ((files[i].fd = fopen(argv[i + 1], "r")) == NULL) {
            perror(argv[i + 1]);
            exit(FILE_ERROR);
        }
        files[i].activated = true;
        files[i].data = malloc(PERIOD_SIZE_IN_BYTES);
        files[i].peak_values = malloc(sizeof(uint16_t) * npeak_values);
        files[i].peak_index = 0;
    }

    uint8_t *data[nfiles];
    uint8_t mixed_data[PERIOD_SIZE_IN_BYTES];

    while (true) {
        uint8_t active_files = 0;
        for (uint8_t i = 0; i < nfiles; i++) {
            if (!files[i].activated) {
                continue;
            }
            if (fread(files[i].data, 1, PERIOD_SIZE_IN_BYTES, files[i].fd) ==
                PERIOD_SIZE_IN_BYTES) {
                data[active_files++] = files[i].data;
                // Save the peak value in this period
                uint16_t peak_value = 0;
                uint16_t *u16data = (uint16_t *)(files[i].data);
                for (int j = 0; j < PERIOD_SIZE_IN_FRAMES * CHANNELS; j++) {
                    if (u16data[j] > peak_value) {
                        peak_value = u16data[j];
                    }
                }
                files[i].peak_values[files[i].peak_index] = peak_value;
                if (++files[i].peak_index % npeak_values == 0) {
                    files[i].peak_index = 0;
                }
            } else {
                files[i].activated = false;
            }
        }

        uint8_t *write_buf;
        if (active_files == 0) {
            break;
        } else if (active_files == 1) {
            write_buf = data[0];
        } else {
            assert(audio_umix16((uint16_t **)data, active_files,
                                (uint16_t *)mixed_data) == 0);
            write_buf = mixed_data;
        }

        if (audio_write(audio_info, (uint8_t *)write_buf,
                        PERIOD_SIZE_IN_FRAMES) < 0) {
            break;
        }
    }

    stop();
}
