#include <stdbool.h>
#include <signal.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sched.h>
#include "audio.h"
#include "timing.h"
#include "jb_table.h"
#include "network_receiver.h"

// |userid:4|timestamp:8| = 12 bytes
#define HEADER_SIZE (4 + 8)

#define SOCKET_ERROR -202
#define AUDIO_ERROR -203

#define FOUR_SECONDS_IN_US (4 * 1000000)

#define DRAIN_BUF_SIZE 32768

#define MAX_JITTER_BUFFER_SIZE 20

void *network_receiver(void *arg) {
  int err;
  int sockfd = -1;
  audio_info_t *audio_info = NULL;
  jb_t *jb_table = NULL;
  
  // Extract parameters
  network_receiver_params_t *receiver_params = (network_receiver_params_t *)arg;
  uint16_t port = receiver_params->port;
  
  // Hardwired audio parameters
  char *pcm_name = "default";
  snd_pcm_stream_t stream = SND_PCM_STREAM_PLAYBACK;
  int mode = SND_PCM_NONBLOCK;
  snd_pcm_format_t format = SND_PCM_FORMAT_S16_LE;
  uint8_t channels = 2;
  uint8_t sample_size_in_bytes = 2;
  uint8_t frame_size_in_bytes = channels * sample_size_in_bytes;
  uint32_t rate_in_hz = 48000;
  snd_pcm_uframes_t period_size_in_frames = 128;
  uint32_t period_size_in_bytes = period_size_in_frames * frame_size_in_bytes;
  uint8_t buffer_multiplicator = 10;
  
  // Create and bind socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket: Socket creation failed");
    exit(SOCKET_ERROR);
  }
  
  struct sockaddr_in src_addr = {0};
  src_addr.sin_family = AF_INET;
  src_addr.sin_port = htons(port);
  src_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  
  if (bind(sockfd, (struct sockaddr *)&src_addr, sizeof(src_addr)) < 0) {
    perror("bind: Binding of socket failed");
    exit(SOCKET_ERROR);
  }
  
  struct timeval zero_timeout = {.tv_usec = 0, .tv_sec = 0};
  struct timeval one_second_timeout = {.tv_usec = 0, .tv_sec = 1};
  uint32_t udp_buf_size = HEADER_SIZE + period_size_in_bytes;
  uint8_t drain_buf[DRAIN_BUF_SIZE];
  
  while (true) {
    bool give_up = false;

    // Waiting for incoming audio
    printf("Waiting for incoming audio...\n");
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(sockfd, &readfds);
    if (select(sockfd + 1, &readfds, 0, 0, NULL) < 0) {
      perror("select: Failed to wait for incoming audio");
      break;
    }
    
    // Open audio device
    if ((err = audio_new(pcm_name, stream, mode, format, channels, rate_in_hz,
                         sample_size_in_bytes, period_size_in_frames,
                         buffer_multiplicator, &audio_info)) < 0) {
      fprintf(stderr, "audio_new: Could not initialize audio: %s\n",
              snd_strerror(err));
      break;
    }
    audio_print_parameters(audio_info);
      
    // Drain socket receive buffer
    while (true) {
      FD_ZERO(&readfds);
      FD_SET(sockfd, &readfds);
      struct timeval timeout = zero_timeout;
      int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
      if (nfds < 0) {
        perror("select: Failed to drain socket receive buffer\n");
        give_up = true;
        break;
      } else if (nfds == 0) {
        break;
      }
      if (recvfrom(sockfd, drain_buf, DRAIN_BUF_SIZE, 0, NULL, NULL) < 0) {
        perror("recvfrom: Failed to drain socket receive buffer\n");
        give_up = true;
        break;
      }
      FD_ZERO(&readfds);
      FD_SET(sockfd, &readfds);
    }
    if (give_up) {
      break;
    } else {
      printf("Socket receive buffer has been drained\n");
    }

    // Read from socket and write to non-blocking audio device
    printf("Receiving audio...\n");
    jb_table = jb_table_new();
    uint64_t userid = 0;
    jb_t *jb = NULL;
    double latency = 0;
    uint64_t last_latency_printout = 0;
    
    while (true) {
      // Wait for incoming socket data (or timeout)
      FD_ZERO(&readfds);
      FD_SET(sockfd, &readfds);
      struct timeval timeout = one_second_timeout;
      int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
      if (nfds < 0) {
        perror("select: Failed to wait for incoming socket data");
        give_up = true;
        break;
      } else if (nfds == 0) {
        break;
      }
      
      // Peek into socket and extract userid
      uint32_t new_userid;
      if (recvfrom(sockfd, &new_userid, sizeof(uint32_t), MSG_PEEK, NULL, NULL) < 0) {
        perror("recvfrom: Failed to peek into socket and extract userid");
        give_up = true;
        break;
      }
      
      // Get jitter buffer
      if (userid != new_userid) {
        if ((jb = jb_table_find(&jb_table, new_userid)) == NULL) {
          jb = jb_new(new_userid);
          assert(jb_table_add(&jb_table, jb) == JB_TABLE_SUCCESS);
        }
        userid = new_userid;
      }
      
      // Prepare new jitter buffer entry
      jb_entry_t *jb_entry;
      if (jb->entries > MAX_JITTER_BUFFER_SIZE) {
        jb_entry = jb_pop(jb);
      } else {
        jb_entry = jb_entry_new(udp_buf_size);
      }            
      
      // Read from socket
      int n;
      if ((n = recvfrom(sockfd, jb_entry->data, udp_buf_size, 0, NULL, NULL)) < 0) {
        perror("recvfrom: Failed to read from socket");
        give_up = true;
        break;
      }
      assert(n == udp_buf_size);
      
      // Add timestamp to jitter buffer entry and insert it into jitter buffer
      // NOTE: The jitter buffer is not used for now! See below.
      uint64_t timestamp;
      memcpy(&timestamp, &jb_entry->data[4], sizeof(uint64_t));
      jb_entry->timestamp = timestamp;
      assert(jb_insert(jb, jb_entry) != 0);
      
      // Calculate latency
      uint64_t now = utimestamp();
      latency = latency * 0.9 + (now - timestamp) * 0.1;
      if (now - last_latency_printout > FOUR_SECONDS_IN_US) {
        printf("Latency: %fms\n", latency / 1000);
        last_latency_printout = now;
      }
      
      // Write to audio device
      // NOTE: This will later on be done in a separate thread which
      // reads from the jitter buffer and writes to audio device
      uint32_t written_frames = 0;
      while (written_frames < period_size_in_frames) {
        snd_pcm_uframes_t frames =
          snd_pcm_writei(audio_info->pcm,
                         &jb_entry->data[HEADER_SIZE +
                                         written_frames * frame_size_in_bytes],
                         period_size_in_frames - written_frames);
        if (frames == -EAGAIN) {
          fprintf(stderr,
                  "snd_pcm_writei: Failed to write to audio device: %s\n",
                  snd_strerror(err));
          break;
        } else if (frames == -EPIPE) {
          fprintf(stderr,
                  "snd_pcm_writei: Failed to write to audio device: %s\n",
                  snd_strerror(err));
          if ((err = snd_pcm_prepare(audio_info->pcm)) < 0) {
            fprintf(stderr,
                    "snd_pcm_prepare: Failed to prepare audio device: %s\n",
                    snd_strerror(err));
          }
          break;
        } else if (frames < 0) {
          fprintf(stderr,
                  "snd_pcm_writei: Failed to write to audio device: %s\n",
                  snd_strerror(err));
          break;
        } else {
          written_frames += frames;
        }
      }
      if (give_up) {
        break;
      }
    }

    printf("No longer receiving audio!\n");

    audio_free(audio_info);
    audio_info = NULL;
    jb_table_free(jb_table);
    jb_table = NULL;
  }

  if (jb_table != NULL) {
    jb_table_free(jb_table);
  }
  if (audio_info != NULL) {
    audio_free(audio_info);
  }
  if (sockfd != -1) {
    close(sockfd);
  }

  return NULL;
}
