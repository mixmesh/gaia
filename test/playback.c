#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <math.h>
#include <alsa/asoundlib.h>
#include "../audio.h"
#include "../globals.h"

// $ ./capture test.dat
// $ ./playback test.dat

#define MAX_FILES 128
#define PEAK_AVERAGE_PERIOD_IN_MS 200

typedef struct {
    FILE *fd;
    char *filename;
    bool activated;
    uint8_t *data;
    uint16_t *peak_values;
    uint16_t peak_index;
    uint16_t peak_average;
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

uint16_t root_mean_square(uint16_t *peak_values, uint16_t n) {
    double sum = 0.0;
    for(uint16_t i = 0; i < n; i++)
        sum += (double)peak_values[i] * peak_values[i];
    return sqrt(sum / n);
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

    uint16_t npeak_values =
        PEAK_AVERAGE_PERIOD_IN_MS / PERIOD_SIZE_IN_MS;

    for (uint8_t i = 0; i < nfiles; i++) {
        if ((files[i].fd = fopen(argv[i + 1], "r")) == NULL) {
            perror(argv[i + 1]);
            exit(FILE_ERROR);
        }
        files[i].filename = argv[i + 1];
        files[i].activated = true;
        files[i].data = malloc(PERIOD_SIZE_IN_BYTES);
        files[i].peak_values = calloc(npeak_values, sizeof(uint16_t));
        files[i].peak_index = 0;
        files[i].peak_average = 0;
    }

    uint8_t *data_to_mix[MAX_MIX_STREAMS];
    uint8_t mixed_data[PERIOD_SIZE_IN_BYTES];
    file_t *active_files[nfiles];

    while (true) {
        uint8_t nactive_files = 0;
        for (uint8_t i = 0; i < nfiles; i++) {
            if (!files[i].activated) {
                continue;
            }
            if (fread(files[i].data, 1, PERIOD_SIZE_IN_BYTES, files[i].fd) ==
                PERIOD_SIZE_IN_BYTES) {
                active_files[nactive_files++] = &files[i];

                // Peak value/average handling
                uint16_t peak_value = 0;
                int16_t *s16data = (int16_t *)(files[i].data);
                for (uint16_t j = 0; j < PERIOD_SIZE_IN_FRAMES * CHANNELS;
                     j++) {
                    uint16_t udata = s16data[j] + 32768;
                    if (udata > peak_value) {
                        peak_value = udata;
                    }
                }
                files[i].peak_values[files[i].peak_index] = peak_value;
                if (++files[i].peak_index % npeak_values == 0) {
                    files[i].peak_index = 0;
                    files[i].peak_average =
                        root_mean_square(files[i].peak_values, npeak_values);
                    //fprintf(stderr, "%s: %d\n", files[i].filename,
                    //        files[i].peak_average);
                }
            } else {
                files[i].activated = false;
            }
        }

        if (nactive_files > 3) {
            int compar(const void *file1, const void *file2) {
                int32_t value1 = (*(file_t **)file1)->peak_average;
                int32_t value2 = (*(file_t **)file2)->peak_average;
                return value2 - value1;
            };
            qsort(active_files, nactive_files, sizeof(file_t *), compar);
        }
        nactive_files =
            (nactive_files < MAX_MIX_STREAMS) ? nactive_files : MAX_MIX_STREAMS;
        for (uint8_t i = 0; i < nactive_files; i++) {
            data_to_mix[i] = active_files[i]->data;
            //fprintf(stderr, "%s (%d) ", active_files[i]->filename, active_files[i]->peak_average);
        }
        //fprintf(stderr, "\n");
        uint8_t *write_buf;
        if (nactive_files == 0) {
            break;
        } else if (nactive_files == 1) {
            write_buf = data_to_mix[0];
        } else {
            assert(audio_smix16((int16_t **)data_to_mix, nactive_files,
                                (int16_t *)mixed_data) == 0);
            write_buf = mixed_data;
        }

        if (audio_write(audio_info, write_buf, PERIOD_SIZE_IN_FRAMES) < 0) {
            break;
        }
    }

    stop();
}
