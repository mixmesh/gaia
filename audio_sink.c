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
  
  // Read from jitter buffer, mix and write to audio device
  while (true) {
    uint8_t *data[256];
    uint8_t ndata = 0;
    
    void mix(jb_t *jb) {
      jb_take_wrlock(jb);
      if (jb->entries > JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS) {
        if (jb->playback == NULL) {
          printf("Initializes playback entry. Reset playback entry.\n");
          reset_playback_delay(jb);
        } else if (jb->playback == jb->tail) {
          printf("Jitter buffer has been exhausted. Reset playback entry.\n");
          reset_playback_delay(jb);
        } else if (jb->playback->seqnum != jb->playback_seqnum) {
          printf("Playback entry %d has been reused by %d. Reset playback \
entry.\n",
                 jb->playback_seqnum, jb->playback->seqnum);
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
              // Seqnum mismatch. Use the old playback entry again!
              assert(jb->playback->prev->seqnum > next_seqnum);
              // NOTE: Disable to remove noise on stdout
              printf("Expected playback entry %d but got %d. Use %d again!\n",
                     next_seqnum, jb->playback->prev->seqnum,
                     jb->playback->seqnum);
              jb->playback->seqnum = next_seqnum;
              jb->playback_seqnum = next_seqnum;
            }
          }
        }
        /*
        // NOTE: This debug printout is too expensive
        uint32_t index = jb_get_index(jb, jb->playback);
        printf("Playback index now is %d out of %d total entries\n",
               index, jb->entries);
        }
        */
        data[ndata++] = &jb->playback->data[HEADER_SIZE];
      }
      jb_release_lock(jb);
    };
                  
    jb_table_take_rdlock(jb_table);
    jb_table_foreach(jb_table, mix);
    jb_table_release_lock(jb_table);
    
    if (ndata > 0) {
      // Open audio device (if needed)
      if (audio_info == NULL) {
        if ((err = audio_new(PCM_NAME, SND_PCM_STREAM_PLAYBACK, 0,
                             FORMAT, CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                             PERIOD_SIZE_IN_FRAMES, BUFFER_MULTIPLICATOR,
                             &audio_info)) < 0) {
          fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
                  snd_strerror(err));
          exit(AUDIO_ERROR);
        }
        audio_print_parameters(audio_info, "sink");
        assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);
        printf("Audio device has been opened for playback\n");
      }
      if (ndata == 1) {
        audio_write(audio_info, data[0], PAYLOAD_SIZE_IN_FRAMES);
      } else if (ndata == 2) {
        assert(SAMPLE_SIZE_IN_BYTES == 2 && FORMAT == SND_PCM_FORMAT_U16_LE);
        uint16_t mix_buf[PERIOD_SIZE_IN_BYTES / SAMPLE_SIZE_IN_BYTES];
        assert(audio_umix16((uint16_t **)data, ndata, mix_buf) == 0);
        audio_write(audio_info, (uint8_t *)mix_buf, PAYLOAD_SIZE_IN_FRAMES);
      } else {
        assert(ndata < 3);
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
