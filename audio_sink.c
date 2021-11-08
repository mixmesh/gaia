#include "audio.h"
#include "jb_table.h"
#include "audio_sink.h"
#include "globals.h"
#include "timing.h"

#define WAIT_IN_MS 100

extern jb_table_t *jb_table;

void *audio_sink(void *arg) {
  int err;

  // Open audio device
  audio_info_t *audio_info = NULL;
  if ((err = audio_new(PCM_NAME, SND_PCM_STREAM_PLAYBACK, 0,
                       FORMAT, CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                       PERIOD_SIZE_IN_FRAMES, BUFFER_MULTIPLICATOR * 2,
                       &audio_info)) < 0) {
    fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
            snd_strerror(err));
    return NULL;
  }
  audio_print_parameters(audio_info, "sink");
  assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);

  // Read from jitter buffer, mix and write to audio device
  uint8_t mix_buf[PAYLOAD_SIZE_IN_BYTES];  
  while (true) {
    bool data_available = false;
    void mix(jb_t *jb) {
      jb_take_wrlock(jb);
      if (jb->entries > JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS) {
        if (jb->playback == NULL) {
          printf("Initializes playback entry\n");
          jb->playback =
            jb_get_entry(jb, JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS);
          assert(jb->playback != NULL);
          jb->playback_seqnum = jb->playback->seqnum;
          data_available = true;
        } else if (jb->playback->seqnum != jb->playback_seqnum) {
          printf("Playback entry %d has been reused by %d\n",
                 jb->playback_seqnum, jb->playback->seqnum);
          jb->playback =
            jb_get_entry(jb, JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS);
          assert(jb->playback != NULL);
          jb->playback_seqnum = jb->playback->seqnum;
          data_available = true;
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
              printf("Expected playback entry %d but got %d. Use %d again!\n",
                     next_seqnum, jb->playback->prev->seqnum,
                     jb->playback->seqnum);
              jb->playback->seqnum = next_seqnum;
              jb->playback_seqnum = next_seqnum;
            }
            data_available = true;
          } else {
            // Jitter buffer is exhausted!
            if (jb->playback_index != 0) {
              printf("Jitter buffer has been exhausted\n");
            }
          }
          /*
          // NOTE: This index checking is too expensive. Remove ASAP!
          uint32_t index = jb_get_index(jb, jb->playback);
          if (!(index == 0 && jb->playback_index == 0) &&
              (index > jb->playback_index + 1 ||
               index + 1 < jb->playback_index)) {
            printf("Playback index now is %d (%d) out of %d total entries\n",
                   index, jb->playback_index, jb->entries);
          }
          jb->playback_index = index;
          */
        }
        // FIXME: Mix
        memcpy(mix_buf, &jb->playback->data[HEADER_SIZE], PAYLOAD_SIZE_IN_BYTES);
      }
      jb_release_lock(jb);
    };
    
    jb_table_take_rdlock(jb_table);    
    jb_table_foreach(jb_table, mix);
    jb_table_release_lock(jb_table);
    
    if (data_available) {
      if (audio_write(audio_info, mix_buf, PAYLOAD_SIZE_IN_FRAMES) < 0) {
        break;
      }
    } else {
      msleep(WAIT_IN_MS);
    }
  }
  
  audio_free(audio_info);

  fprintf(stderr, "audio_sink is shutting down!!!\n");
  exit(3);
  
  return NULL;
}

