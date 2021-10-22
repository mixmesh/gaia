#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <alsa/asoundlib.h>
#include "jb.h"
#include "jb_table.h"
#include "bits.h"
#include "audio.h"
#include "timing.h"

#define DEFAULT_HOST "127.0.0.1"
#define DEFAULT_PORT 54382

// |userid:4|seqnum:4|timestamp:4|
#define HEADER_SIZE (4 + 4 + 4)

#define SOCKET_ERROR 1
#define BIND_ERROR 2

#define MAX_JITTER_BUFFER_ENTRIES 20

void usage(char *command, int status) {
  fprintf(stderr, "Usage: %s [port]\n", command);
  exit(status);
}

audio_info_t *audio_info = NULL;
int sockfd = -1;
uint8_t *udp_buf = NULL;

void sigint(int sig) {
  if (audio_info != NULL) {
    audio_free(audio_info);
  }
  if (sockfd != -1) {
    close(sockfd);
  }
  if (udp_buf != NULL) {
    free(udp_buf);
  }
  exit(1);
}

void start_server(uint16_t port) {
  // Handle Ctrl-c
  signal(SIGINT, sigint);
  // Open audio device
  int err;
  if ((err = audio_new("default", SND_PCM_STREAM_PLAYBACK,
                       SND_PCM_FORMAT_MU_LAW, 1, 8000, 1, 40,
                       3, &audio_info)) < 0) {
    fprintf(stderr, "could not initialize audio: %s\n", snd_strerror(err));
    exit(1);
  }
  audio_print_info(audio_info);
  // Create socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket creation failed");
    exit(SOCKET_ERROR);
  }
  struct sockaddr_in servaddr;
  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = INADDR_ANY;
  servaddr.sin_port = htons(port);
  // Bind socket
  if (bind(sockfd, (const struct sockaddr *)&servaddr,
           sizeof(servaddr)) < 0) {
    perror("bind failed");
    exit(BIND_ERROR);
  }
  // Receive loop
  uint32_t userid = 0;
  jb_t *jb = NULL;
  jb_t *jb_table = jb_table_new();
  uint32_t safe_udp_buf_size = HEADER_SIZE + audio_info->period_size_in_bytes * 2;
  udp_buf = malloc(safe_udp_buf_size);
  uint32_t udp_packet_counter = 0;
  uint32_t audio_data_size = 0;
  uint32_t latency = 0;
  while (true) {
    // Read UDP packet:
    // |userid:4|seqnum:4|timestamp:4|audio:(received_bytes-HEADER_SIZE)|
    ssize_t received_bytes =
      recvfrom(sockfd, udp_buf, safe_udp_buf_size, 0, NULL, NULL);
    assert(received_bytes > HEADER_SIZE);
    uint32_t new_userid =
      udp_buf[0] + (udp_buf[1] << 8) + (udp_buf[2] << 16) + (udp_buf[3] << 24);
    uint32_t seqnum =
      udp_buf[4] + (udp_buf[5] << 8) + (udp_buf[6] << 16) + (udp_buf[7] << 24);
    uint32_t timestamp =
      udp_buf[8] + (udp_buf[9] << 8) + (udp_buf[10] << 16) +
      (udp_buf[11] << 24);
    // Print measured latency each 50 udp packet
    latency = latency * 0.9 + 0.1 * (get_timestamp() - timestamp);
    if (udp_packet_counter++ % 50 == 0) {
      printf("measured latency: %u ms\n", latency / 1000);
    }
    // Get jitter buffer (create if needed)    
    if (jb == NULL || new_userid != userid) {
      userid = new_userid;
      // Start fulhack
      char name[32];
      sprintf(name, "%d", userid);
      // Stop fulhack
      if ((jb = jb_table_find(&jb_table, name)) == NULL) {
        jb = jb_new(name);
        jb_table_add(&jb_table, jb);
      }
      audio_data_size = received_bytes - HEADER_SIZE;
    }
    if (jb->head != NULL && seqnum != jb->head->index + 1) {
      printf("expected sequence number %d but got %d\n", jb->head->index + 1,
             seqnum);
    }
    /*
    printf("%u : %u : %u : %lu : %d\n", userid, seqnum, timestamp,
           received_bytes, audio_data_size);
    */
    // Insert jitter buffer entry
    if (jb->tail == NULL || seqnum > jb->tail->index ) {
      // Prepare new jitter buffer entry
      jb_entry_t *jb_entry;
      if (jb->entries == MAX_JITTER_BUFFER_ENTRIES) {
        jb_entry = jb_pop(jb);
      } else {
          assert(audio_data_size != 0);
          jb_entry = jb_entry_new(audio_data_size);
      }
      jb_entry->index = seqnum;
      memcpy(jb_entry->data, &udp_buf[HEADER_SIZE], audio_data_size);
      // Insert!
      uint8_t result = jb_insert(jb, jb_entry);
      assert(CHK_FLAG(result,
                      HEAD_INSERTED|TAIL_INSERTED|INTERMEDIATE_INSERTED|
                      FIRST_INSERTED));
      if (CHK_FLAG(result, ALREADY_EXISTS)) {
        jb_entry_free(jb_entry);
        printf("D\n");
      } else if (CHK_FLAG(result, TAIL_INSERTED)) {
        printf(">\n");
      } else if (CHK_FLAG(result, HEAD_INSERTED)) {
        //printf("<\n");
      } else if (CHK_FLAG(result, FIRST_INSERTED)) {
        printf("0\n");
      } else if (CHK_FLAG(result, INTERMEDIATE_INSERTED)) {
        printf(".\n");
      } else {
        assert(false);
      }
      // Playback audio data directly. Fulhack.
      snd_pcm_sframes_t received_frames =
        audio_data_size / audio_info->channels / audio_info->sample_size_in_bytes;
      snd_pcm_sframes_t frames =
        snd_pcm_writei(audio_info->handle, &udp_buf[HEADER_SIZE],
                       received_frames);
      if (frames == -EPIPE) {
        printf("underrun occurred\n");
        snd_pcm_prepare(audio_info->handle);
      } else if (frames < 0) {
        fprintf(stderr, "error from writei: %s\n", snd_strerror(frames));
      } else if (frames != received_frames) {
        fprintf(stderr, "short write, write %ld frames\n", frames);
      }
    } else {
      printf("I\n");
    }
  }
}

int main (int argc, char *argv[]) {
  if (argc > 2) {
    usage(argv[0], 1);
  }
  uint16_t port = DEFAULT_PORT;
  if (argc == 2) {
    char *endptr;
    port = strtol(argv[1], &endptr, 10);
    if (strlen(endptr) != 0) {
      usage(argv[0], 2);
    }
  }
  start_server(port);
  return 0;
}
