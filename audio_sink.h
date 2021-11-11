#ifndef _AUDIO_SINK_H_
#define _AUDIO_SINK_H_

typedef struct {
    char* pcm_name;
} audio_sink_params_t;

void *audio_sink(void *arg);

#endif
