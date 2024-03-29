#include <stdbool.h>
#include <opus/opus.h>
#include "audio.h"
#include "bits.h"
#include "globals.h"
#include "timing.h"
#include "network_sender.h"
#include "audio.h"
#include "gaia_utils.h"
#include "threadlib.h"

extern bool kill_network_sender;
extern FILE* LOG_FD;

void *network_sender(void *arg) {
    int err;

    // Parameters
    network_sender_params_t *params = (network_sender_params_t *)arg;

    // Create socket
    int sockfd = -1;
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket: Socket creation failed");
        int retval = SOCKET_ERROR;
        thread_exit(&retval);
    }

    // Resize socket send buffer
    /*
    // NOTE: This was a bad idea for some reason. Disable for now.
    int snd_buf_size = PERIOD_SIZE_IN_BYTES * BUFFER_PERIODS;
    assert(setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &snd_buf_size,
    sizeof(snd_buf_size)) == 0);
    */

    // Make socket non-blocking
    int status_flags = fcntl(sockfd, F_GETFL, 0);
    if (status_flags < 0) {
        perror("fcntl: Socket could not be made non-blocking");
        int retval = SOCKET_ERROR;
        thread_exit(&retval);
    }
    if (fcntl(sockfd, F_SETFL, status_flags | O_NONBLOCK) < 0) {
        perror("fcntl: Socket could not be made non-blocking");
        int retval = SOCKET_ERROR;
        thread_exit(&retval);
    }

    // Create destination addresses
    struct sockaddr_in addrs[params->naddr_ports];
    for (int i = 0; i < params->naddr_ports; i++) {
        memset(&addrs[i], 0, sizeof(addrs[i]));
        addrs[i].sin_family = AF_INET;
        addrs[i].sin_addr.s_addr = params->addr_ports[i].addr;
        addrs[i].sin_port = htons(params->addr_ports[i].port);
    }

    // Create Opus encoders (if enabled)
    OpusEncoder *opus_encoder = NULL;
    if (params->opus_enabled) {
        opus_encoder = opus_encoder_create(RATE_IN_HZ, CHANNELS,
                                           OPUS_APPLICATION_AUDIO, &err);
        if (err < 0) {
            ERRORF("opus_encoder_create: Creation failed: %s",
                   opus_strerror(err));
        }
        assert(err == 0);
        opus_encoder_ctl(opus_encoder,
                         OPUS_SET_COMPLEXITY(OPUS_COMPLEXITY));
    }

    // Open audio device
    audio_info_t *audio_info;

    if ((err = audio_new(params->pcm_name, SND_PCM_STREAM_CAPTURE, 0,
                         FORMAT, CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                         PERIOD_SIZE_IN_FRAMES, BUFFER_PERIODS,
                         &audio_info)) < 0) {
        ERRORF("audio_new: %s", snd_strerror(err));
        int retval = AUDIO_ERROR;
        thread_exit(&retval);
    }
    audio_print_parameters(audio_info, "sender");
    assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);

    INFOF("Period size is %d bytes (%dms)", PERIOD_SIZE_IN_BYTES,
          PERIOD_SIZE_IN_MS);

    // Allocate memory for UDP buffer
    ssize_t udp_max_buf_size;
    if (params->opus_enabled) {
        udp_max_buf_size = HEADER_SIZE + OPUS_MAX_PACKET_LEN_IN_BYTES;
    } else {
        udp_max_buf_size = HEADER_SIZE + PERIOD_SIZE_IN_BYTES;
    }
    uint8_t *udp_buf = malloc(udp_max_buf_size);

    // Add gaia-id to UDP buffer header
    uint32_t peer_id_nl = htonl(params->peer_id);
    memcpy(udp_buf, &peer_id_nl, sizeof(uint32_t));

    // Create a period buffer
    uint8_t period_buf[PERIOD_SIZE_IN_BYTES];

    // Set buffer header flags
    uint8_t flags = 0b00000000;
    if (params->opus_enabled) {
        udp_buf[18] = SET_FLAG(flags, OPUS_ENABLED_FLAG);
    } else {
        udp_buf[18] = flags;
    }

    // Let sequence number start with 1 (zero is reserved)
    uint32_t seqnum = 1;

    // Read from audio device and write to socket
    INFOF("Sending audio...");
    while (!kill_network_sender) {
        // Add timestamp to UDP buffer header
        uint64_t timestamp_nll = htonll(utimestamp());
        memcpy(&udp_buf[4], &timestamp_nll, sizeof(uint64_t));

        // Add seqnum to UDP buffer header
        uint32_t seqnum_nl = htonl(seqnum);
        memcpy(&udp_buf[12], &seqnum_nl, sizeof(uint32_t));

        // Add audio packet to UDP buffer
        int packet_len;
        if (params->opus_enabled) {
            if ((err = audio_read(audio_info, period_buf,
                                  PERIOD_SIZE_IN_FRAMES)) < 0) {
                ERRORF("audio_read: %s", snd_strerror(err));
                continue;
            }
            if ((packet_len = opus_encode(opus_encoder,
                                          (opus_int16 *)period_buf,
                                          PERIOD_SIZE_IN_FRAMES,
                                          &udp_buf[HEADER_SIZE],
                                          OPUS_MAX_PACKET_LEN_IN_BYTES)) < 0) {
                ERRORF("opus_encode: %s", opus_strerror(packet_len));
                break;
            }
        } else {
            if ((err = audio_read(audio_info, &udp_buf[HEADER_SIZE],
                                  PERIOD_SIZE_IN_FRAMES)) < 0) {
                ERRORF("audio_read: %s", snd_strerror(err));
                continue;
            }
            packet_len = PERIOD_SIZE_IN_BYTES;
        }

        // Add packet length to UDP buffer header
        uint16_t packet_len_ns = htons((uint16_t)packet_len);
        memcpy(&udp_buf[16], &packet_len_ns, sizeof(uint16_t));

        // Write to non-blocking socket
        ssize_t udp_buf_size = HEADER_SIZE + packet_len;
        for (int i = 0; i < params->naddr_ports; i++) {
            ssize_t n = sendto(sockfd, udp_buf, udp_buf_size, 0,
                               (struct sockaddr *)&addrs[i], sizeof(addrs[i]));
            if (n < 0) {
                perror("sendto: Failed to write to socket");
            } else if (n != udp_buf_size) {
                ERRORF("sendto: Too few bytes written to socket!");
            }
        }

        seqnum++;
    }

    INFOF("network_sender is shutting down!!!");
    audio_free(audio_info);
    free(udp_buf);
    close(sockfd);
    if (params->opus_enabled) {
        opus_encoder_destroy(opus_encoder);
    }
    int retval = NETWORK_SENDER_DIED;
    thread_exit(&retval);
    return NULL;
}
