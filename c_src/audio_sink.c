#include <math.h>
#include "audio.h"
#include "bits.h"
#include "jb_table.h"
#include "audio_sink.h"
#include "globals.h"
#include "timing.h"
#include "gaia_utils.h"

extern jb_table_t *jb_table;
extern bool kill_audio_sink;
extern uint8_t *playback_packet;
extern thread_mutex_t *playback_packet_mutex;

#define MAX_SILENCE_CYCLES 4

void reset_playback_delay(jb_t *jb) {
    jb->playback = jb_get_entry(jb, JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS);
    assert(jb->playback != NULL);
    jb->playback_seqnum = jb->playback->seqnum;
}

void *audio_sink(void *arg) {
    int err;
    audio_info_t *audio_info = NULL;
    uint8_t *packet[MAX_MEMBERS], npackets;

    // Parameters
    audio_sink_params_t *params = (audio_sink_params_t *)arg;

    void step_playback_entry(jb_t *jb) {
        jb_take_wrlock(jb);
        if (jb->nentries > JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS) {
            bool skip_packet = false;
            if (jb->playback == NULL) {
                INFOF("Jitter buffer (re)initializes playback for gaia-id %u",
                      jb->gaia_id);
                reset_playback_delay(jb);
            } else if (jb->playback == jb->tail) {
                DEBUGF("Jitter buffer playback is exhausted for gaia-id %u",
                       jb->gaia_id);
                jb->exhausted = true;
                skip_packet = true;
            } else if (jb->playback->seqnum != jb->playback_seqnum) {
                INFOF("Jitter buffer playback has wrapped around for gaia-id \
%u",
                      jb->gaia_id);
                reset_playback_delay(jb);
            } else {
                // Step playback entry
                uint32_t next_seqnum = jb->playback->seqnum + 1;
                if (jb->playback->prev != NULL) {
                    if (jb->playback->prev->seqnum == next_seqnum) {
                        // All is good
                        jb->playback = jb->playback->prev;
                        jb->playback_seqnum = next_seqnum;
                    } else {
                        // Seqnum mismatch. Use the old playback entry
                        // again!
                        assert(jb->playback->prev->seqnum > next_seqnum);
                        INFOF("Jitter buffer for gaia-id %u expected playback \
entry %u but got %u (%u will be reused as %u!)",
                              jb->gaia_id, next_seqnum,
                              jb->playback->prev->seqnum,
                              jb->playback->prev->seqnum, next_seqnum);
                        jb->playback->seqnum = next_seqnum;
                        jb->playback_seqnum = next_seqnum;
                    }
                }
            }

            if (!skip_packet) {
                if (CHK_FLAG(jb->playback->udp_buf[18], OPUS_ENABLED_FLAG)) {
                    uint16_t packet_len =
                        ntohs(*(uint16_t *)&jb->playback->udp_buf[16]);
                    int frames;
                    if ((frames =
                         opus_decode(jb->opus_decoder,
                                     &jb->playback->udp_buf[HEADER_SIZE],
                                     packet_len,
                                     (opus_int16 *)jb->playback->period_buf,
                                     OPUS_MAX_PACKET_LEN_IN_BYTES,
                                     0)) < 0) {
                        ERRORF("Failed to Opus decode: %s",
                               opus_strerror(frames));
                    } else {
                        assert(frames == PERIOD_SIZE_IN_FRAMES);
                        packet[npackets++] = jb->playback->period_buf;
                    }
                } else {
                    packet[npackets++] = &jb->playback->udp_buf[HEADER_SIZE];
                }
            }

            /*
            // NOTE: This debug printout is too expensive
            uint32_t index = jb_get_index(jb, jb->playback);
            INFOF("Playback index now is %u out of %u total entries",
            index, jb->entries);
            */
        }
        jb_release_wrlock(jb);
    };

    struct timespec period_size_as_tsp =
        {
         .tv_sec = 0,
         .tv_nsec = PERIOD_SIZE_IN_NS
        };
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);

    bool holding_playback_packet_mutex = false;
    uint16_t silence_cycles = 0;

    // Read from jitter buffer, mix and write to audio device
    while (!kill_audio_sink) {
        if (!holding_playback_packet_mutex) {
            assert(thread_mutex_lock(playback_packet_mutex) == 0);
            holding_playback_packet_mutex = true;
        }

        npackets = 0;
        jb_table_take_rdlock(jb_table);
        jb_table_foreach(jb_table, step_playback_entry);
        jb_table_release_rdlock(jb_table);

        if (npackets > 0) {
            // Open audio device (if needed)
            if (params->playback_audio && audio_info == NULL) {
                if ((err = audio_new(params->pcm_name, SND_PCM_STREAM_PLAYBACK,
                                     SND_PCM_NONBLOCK, FORMAT, CHANNELS,
                                     RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                                     PERIOD_SIZE_IN_FRAMES, BUFFER_PERIODS,
                                     &audio_info)) < 0) {
                    ERRORF("audio_new: %s", snd_strerror(err));
                    int retval = AUDIO_ERROR;
                    thread_exit(&retval);
                }
                audio_print_parameters(audio_info, "sink");
                assert(PERIOD_SIZE_IN_FRAMES ==
                       audio_info->period_size_in_frames);
                INFOF("Audio device has been opened for playback");
            }

            // Prepare playback packet
            if (npackets == 1) {
                memcpy(playback_packet, packet[0], PERIOD_SIZE_IN_BYTES);
            } else {
                npackets =
                    (npackets < MAX_MIX_STREAMS) ? npackets : MAX_MIX_STREAMS;
                assert(audio_smix16((int16_t **)packet,
                                    npackets, (int16_t *)playback_packet,
                                    PERIOD_SIZE_IN_FRAMES, CHANNELS) == 0);
            }

            // Sleep (very carefully)
            struct timespec next_time;
            timespecadd(&time, &period_size_as_tsp, &next_time);
            struct timespec rem;
            assert(clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
                                   &next_time, &rem) == 0);
            assert(rem.tv_nsec == 0);
            memcpy(&time, &next_time, sizeof(struct timespec));

            if (params->playback_audio) {
                if ((err = audio_non_blocking_write(
                               audio_info, playback_packet,
                               PERIOD_SIZE_IN_FRAMES)) < 0) {
                    ERRORF("audio_non_blocking_write: %s", snd_strerror(err));
                }
            }

            assert(thread_mutex_unlock(playback_packet_mutex) == 0);
            holding_playback_packet_mutex = false;
            silence_cycles = 0;
        } else {
            INFOF("No data available in jitter buffers");
            // Exponential fallback
            if (silence_cycles > MAX_SILENCE_CYCLES) {
                if (params->playback_audio && audio_info != NULL) {
                    audio_free(audio_info);
                    audio_info = NULL;
                    INFOF("Audio device has been closed for playback");
                }
                uint32_t sleep_time = (PERIOD_SIZE_IN_MS * 2) * pow(2, MAX_SILENCE_CYCLES);
                INFOF("Audio sink sleeps for %ums", sleep_time);
                msleep(sleep_time);
            } else {
                uint32_t sleep_time = (PERIOD_SIZE_IN_MS * 2) * pow(2, silence_cycles++);
                INFOF("Audio sink sleeps for %ums", sleep_time);
                msleep(sleep_time);
                if (params->playback_audio && audio_info != NULL) {
                    snd_pcm_recover(audio_info->pcm, 0, 1);
                }
            }
        }
    }

    INFOF("audio_sink is shutting down!!!");
    if (params->playback_audio && audio_info != NULL) {
        audio_free(audio_info);
    }
    int retval = AUDIO_SINK_DIED;
    thread_exit(&retval);
    return NULL;
}
