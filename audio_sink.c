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
        if (jb->playback == NULL || jb->playback->seqnum != jb->seqnum) {
          // Initialize playback entry
          printf("Initializes playback entry...");
          jb->playback =
            jb_get_entry(jb, JITTER_BUFFER_PLAYBACK_DELAY_IN_PERIODS);
          assert(jb->playback != NULL);
          jb->seqnum = jb->playback->seqnum;
          data_available = true;
        } else {
          // Step playback entry
          uint32_t next_seqnum = jb->playback->seqnum + 1;
          if (jb->playback->prev != NULL) {
            if (jb->playback->prev->seqnum == next_seqnum) {
              // All is good
              jb->playback = jb->playback->prev;
              jb->seqnum = next_seqnum;
            } else {
              // Seqnum mismatch. Use the old playback entry again!
              printf("Replay playback entry to cover for missing data...");
              assert(jb->playback->prev->seqnum < next_seqnum);
              jb->playback->seqnum = next_seqnum;
              jb->seqnum = next_seqnum;
            }
            data_available = true;
          } else {
            // Jitter buffer is exhausted!
            printf("Jitter buffer has been exhausted");
          }
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
      snd_pcm_uframes_t frames =
        audio_write(audio_info, mix_buf, PAYLOAD_SIZE_IN_FRAMES);
      if (frames < 0 && frames != AUDIO_NOT_RECOVERED) {
        break;
      }
    } else {
      msleep(WAIT_IN_MS);
    }
  }
  
  audio_free(audio_info);
  
  return NULL;
}

/*
http://www.vttoth.com/CMS/index.php/technical-notes/68
https://stackoverflow.com/a/25102339


int a = 111; // first sample (-32768..32767)
int b = 222; // second sample
int m; // mixed result will go here

// Make both samples unsigned (0..65535)
a += 32768;
b += 32768;

// Pick the equation
if ((a < 32768) || (b < 32768)) {
    // Viktor's first equation when both sources are "quiet"
    // (i.e. less than middle of the dynamic range)
    m = a * b / 32768;
} else {
    // Viktor's second equation when one or both sources are loud
    m = 2 * (a + b) - (a * b) / 32768 - 65536;
}

// Output is unsigned (0..65536) so convert back to signed (-32768..32767)
if (m == 65536) m = 65535;
m -= 32768;
*/
