#ifndef _AUDIO_SINK_H_
#define _AUDIO_SINK_H_

#include <stdbool.h>

typedef struct {
    char* pcm_name;
    bool opus_enabled;
    bool playback_audio;
} audio_sink_params_t;

void *audio_sink(void *arg);

#endif
