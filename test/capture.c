#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include "../audio.h"
#include "../globals.h"

// $ ./capture test.dat
// $ ./playback test.dat

audio_info_t *audio_info = NULL;
FILE *fd = NULL;

void stop() {
    if (fd != NULL) {
        fclose(fd);
    }
    if (audio_info != NULL) {
        audio_free(audio_info);
    }
    exit(0);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        exit(ARG_ERROR);
    }

    if (signal(SIGINT, stop) == SIG_ERR) {
        perror("signal");
        exit(INTERNAL_ERROR);
    }

    int err;
    if ((err = audio_new(DEFAULT_PCM_NAME, SND_PCM_STREAM_CAPTURE, 0, FORMAT,
                         CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                         PERIOD_SIZE_IN_FRAMES, BUFFER_MULTIPLICATOR,
                         &audio_info)) < 0) {
        fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
                snd_strerror(err));
        exit(AUDIO_ERROR);
    }
    audio_print_parameters(audio_info, "capture");
    assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);

    if ((fd = fopen(argv[1], "w")) == NULL) {
        perror("fopen: Could not open filename");
        exit(FILE_ERROR);
    }

    uint8_t buf[PERIOD_SIZE_IN_BYTES];

    while (true) {
        if (audio_read(audio_info, buf, PERIOD_SIZE_IN_FRAMES) < 0) {
            break;
        }

        if (fwrite(&buf, 1, PERIOD_SIZE_IN_BYTES, fd) != PERIOD_SIZE_IN_BYTES) {
            perror("fwrite");
            break;
        }
    }

    stop();
}
