#include "audio.h"
#include "jb_table.h"
#include "audio_sink.h"
#include "globals.h"
#include "timing.h"

#define WAIT_IN_MS 2000

extern jb_table_t *jb_table;

void reset_playback_delay(jb_t *jb) {
    jb->playback = jb_get_entry(jb, JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS);
    assert(jb->playback != NULL);
    jb->playback_seqnum = jb->playback->seqnum;
}

void *audio_sink(void *arg) {
    int err;
    audio_info_t *audio_info = NULL;
    uint8_t *packet[MAX_USERS];

    // Parameters
    audio_sink_params_t *params = (audio_sink_params_t *)arg;

    // Read from jitter buffer, mix and write to audio device
    while (true) {
        uint8_t npackets = 0;
        void step_playback_entry(jb_t *jb) {
            jb_take_wrlock(jb);
            if (jb->nentries > JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS) {
                bool skip_packet = false;
                if (jb->playback == NULL) {
                    printf("Jitter buffer (re)initializes playback for userid \
%d\n",
                           jb->userid);
                    reset_playback_delay(jb);
                } else if (jb->playback == jb->tail) {
                    printf("Jitter buffer playback is exhausted for userid \
%d\n",
                           jb->userid);
                    jb->exhausted = true;
                    skip_packet = true;
                } else if (jb->playback->seqnum != jb->playback_seqnum) {
                    printf("Jitter buffer playback has wrapped around for \
userid %d.\n",
                           jb->userid);
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
                            // NOTE: Disable to remove noise on stdout
                            printf("Jitter buffer for userid %d expected \
playback entry %d but got %d (%d will be reused as %d!)\n",
                                   jb->userid, next_seqnum,
                                   jb->playback->prev->seqnum,
                                   jb->playback->prev->seqnum, next_seqnum);
                            jb->playback->seqnum = next_seqnum;
                            jb->playback_seqnum = next_seqnum;
                        }
                    }
                }

                if (!skip_packet) {
                    if (params->opus_enabled) {
                        uint16_t packet_len =
                            *(uint16_t *)&jb->playback->udp_buf[16];
                        int frames;
                        if ((frames =
                             opus_decode(jb->opus_decoder,
                                         &jb->playback->udp_buf[HEADER_SIZE],
                                         packet_len,
                                         (opus_int16 *)jb->playback->period_buf,
                                         OPUS_MAX_PACKET_LEN_IN_BYTES,
                                         0)) < 0) {
                            fprintf(stderr, "Failed to Opus decode: %s\n",
                                    opus_strerror(frames));
                        }
                        assert(frames == PERIOD_SIZE_IN_FRAMES);
                        packet[npackets++] = jb->playback->period_buf;
                    } else {
                        packet[npackets++] = &jb->playback->udp_buf[HEADER_SIZE];
                    }
                }

                /*
                // NOTE: This debug printout is too expensive
                uint32_t index = jb_get_index(jb, jb->playback);
                printf("Playback index now is %d out of %d total entries\n",
                    index, jb->entries);
            }
                */
            }
            jb_release_lock(jb);
        };

        jb_table_take_rdlock(jb_table);
        jb_table_foreach(jb_table, step_playback_entry);
        jb_table_release_lock(jb_table);

        if (npackets > 0) {
            // Open audio device (if needed)
            if (audio_info == NULL) {
                if ((err = audio_new(params->pcm_name, SND_PCM_STREAM_PLAYBACK,
                                     0, FORMAT, CHANNELS, RATE_IN_HZ,
                                     SAMPLE_SIZE_IN_BYTES,
                                     PERIOD_SIZE_IN_FRAMES,
                                     BUFFER_MULTIPLICATOR, &audio_info)) < 0) {
                    fprintf(stderr, "audio_new: Could not initialize audio: \
%s\n",
                            snd_strerror(err));
                    exit(AUDIO_ERROR);
                }
                audio_print_parameters(audio_info, "sink");
                assert(PERIOD_SIZE_IN_FRAMES ==
                       audio_info->period_size_in_frames);
                printf("Audio device has been opened for playback\n");
            }
            if (npackets == 1) {
                audio_write(audio_info, packet[0], PERIOD_SIZE_IN_FRAMES);
            } else {
                uint8_t mixed_packet[PERIOD_SIZE_IN_BYTES];
                npackets = (npackets < MAX_MIX_STREAMS) ? npackets : MAX_MIX_STREAMS;
                assert(audio_smix16((int16_t **)packet,
                                    npackets, (int16_t *)mixed_packet,
                                    PERIOD_SIZE_IN_FRAMES, CHANNELS) == 0);
                audio_write(audio_info, mixed_packet, PERIOD_SIZE_IN_FRAMES);
            }
        } else {
            fprintf(stderr, "No data available in jitter buffers\n");
            // Close audio device and wait a bit
            if (audio_info != NULL) {
                audio_free(audio_info);
                audio_info = NULL;
            }
            fprintf(stderr, "Audio sink sleeps for %dms\n", WAIT_IN_MS);
            msleep(WAIT_IN_MS);
        }
    }

    fprintf(stderr, "audio_sink is shutting down!!!\n");
    audio_free(audio_info);
    exit(AUDIO_SINK_DIED);
}
