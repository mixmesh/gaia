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

typedef struct {
    FILE *fd;
    char *filename;
    uint8_t *cache;
    uint32_t cache_index;
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
        free(files[i].cache);
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

    snd_pcm_uframes_t chunk_size_in_frames = PERIOD_SIZE_IN_FRAMES * 4;
    uint32_t chunk_size_in_bytes = chunk_size_in_frames * FRAME_SIZE_IN_BYTES;
    uint32_t chunk_size_in_ms =  chunk_size_in_frames / (RATE_IN_HZ / 1000.0);
    uint32_t file_cache_size = chunk_size_in_bytes * 10;
    uint32_t peak_average_period_in_ms = 5000;

    printf("chunk_size_in_frames: %ld\n", chunk_size_in_frames);
    printf("chunk_size_in_bytes: %d\n", chunk_size_in_bytes);
    printf("chunk_size_in_ms: %d\n", chunk_size_in_ms);
    printf("file_cache_size: %dkb\n", file_cache_size / 1024);
    printf("peak_average_period_in_ms: %d\n", peak_average_period_in_ms);

    nfiles = argc - 1;

    if (signal(SIGINT, stop) == SIG_ERR) {
        perror("signal");
        exit(INTERNAL_ERROR);
    }

    int err;
    if ((err = audio_new(DEFAULT_PCM_NAME, SND_PCM_STREAM_PLAYBACK, 0, FORMAT,
                         CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                         PERIOD_SIZE_IN_FRAMES, BUFFER_MULTIPLICATOR,
                         &audio_info)) < 0) {
        fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
                snd_strerror(err));
        exit(AUDIO_ERROR);
    }
    audio_print_parameters(audio_info, "playback");
    assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);

    uint16_t npeak_values = peak_average_period_in_ms / chunk_size_in_ms;

    // Open all files and prepare for battle
    for (uint8_t i = 0; i < nfiles; i++) {
        if ((files[i].fd = fopen(argv[i + 1], "r")) == NULL) {
            perror(argv[i + 1]);
            exit(FILE_ERROR);
        }

        // Check file size
        fseek(files[i].fd, 0L, SEEK_END);
        if (ftell(files[i].fd) < file_cache_size) {
            fprintf(stderr, "%s is smaller than %d bytes\n", files[i].filename,
                    file_cache_size);
            fclose(files[i].fd);
            exit(FILE_ERROR);
        }
        rewind(files[i].fd);

        files[i].filename = argv[i + 1];
        files[i].cache = malloc(file_cache_size);
        files[i].cache_index = file_cache_size;
        files[i].peak_values = calloc(npeak_values, sizeof(uint16_t));
        files[i].peak_index = 0;
        files[i].peak_average = 0;
    }

    uint8_t *data[nfiles];
    uint8_t mixed_data[chunk_size_in_bytes];

    // Read from files, mix and write to audio device
    while (true) {
        for (uint8_t i = 0; i < nfiles; i++) {
            // Cache file to RAM (if needed)
            if (files[i].cache_index == file_cache_size) {
                size_t read_bytes =
                    fread(files[i].cache, 1, file_cache_size, files[i].fd);
                if (read_bytes < file_cache_size) {
                    if (feof(files[i].fd)) {
                        printf("Reached end of file in %s. Start from \
scratch!\n",
                               files[i].filename);
                        rewind(files[i].fd);
                        uint32_t more_bytes = file_cache_size - read_bytes;
                        if (fread(&files[i].cache[read_bytes], 1, more_bytes,
                                  files[i].fd) < more_bytes) {
                            perror("fread");
                            break;
                        }
                    } else {
                        perror("fread");
                        break;
                    }
                }
                files[i].cache_index = 0;
            }

            // Peak value/average handling
            uint16_t peak_value = 0;
            int16_t *s16data = (int16_t *)&files[i].cache[files[i].cache_index];
            for (uint16_t j = 0; j < chunk_size_in_frames * CHANNELS;
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
            }
        }

        if (nfiles > 2) {
            int compar(const void *file1, const void *file2) {
                int32_t value1 = ((file_t *)file1)->peak_average;
                int32_t value2 = ((file_t *)file2)->peak_average;
                return value2 - value1;
            };
            qsort(files, nfiles, sizeof(file_t), compar);
        }

        uint8_t nfiles_to_mix =
            (nfiles < MAX_MIX_STREAMS) ? nfiles : MAX_MIX_STREAMS;
        for (uint8_t i = 0; i < nfiles_to_mix; i++) {
            data[i] = &files[i].cache[files[i].cache_index];
        }

        if (nfiles_to_mix == 1) {
            if (audio_write(audio_info, data[0], chunk_size_in_frames) < 0) {
                break;
            }
        } else {
            assert(audio_smix16((int16_t **)data, nfiles_to_mix,
                                (int16_t *)mixed_data,
                                chunk_size_in_frames, CHANNELS) == 0);
            if (audio_write(audio_info, mixed_data,
                            chunk_size_in_frames) < 0) {
                break;
            }
        }

        for (uint8_t i = 0; i < nfiles; i++) {
            files[i].cache_index += chunk_size_in_bytes;
        }
    }

    stop();
}
