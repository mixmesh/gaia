#include <stdbool.h>
#include <opus/opus.h>
#include "audio.h"
#include "globals.h"
#include "timing.h"
#include "network_sender.h"
#include "audio.h"
#include "gaia_utils.h"
#include "threadlib.h"

extern bool kill_network_sender;

void *network_sender(void *arg) {
    int err;

    // Parameters
    network_sender_params_t *params = (network_sender_params_t *)arg;

    // Create socket
    int sockfd = -1;
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
#ifdef DEBUG
        perror("socket: Socket creation failed");
#endif
        int retval = SOCKET_ERROR;
        thread_exit(&retval);
    }

    // Resize socket send buffer
    /*
    // NOTE: This was a bad idea for some reason. Disable for now.
    int snd_buf_size = PERIOD_SIZE_IN_BYTES * BUFFER_MULTIPLICATOR;
    assert(setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &snd_buf_size,
    sizeof(snd_buf_size)) == 0);
    */

    // Make socket non-blocking
    int flags = fcntl(sockfd, F_GETFL, 0);
    if (flags < 0) {
#ifdef DEBUG
        perror("fcntl: Socket could not be made non-blocking");
#endif
        int retval = SOCKET_ERROR;
        thread_exit(&retval);
    }
    if (fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) < 0) {
#ifdef DEBUG
        perror("fcntl: Socket could not be made non-blocking");
#endif
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
            DEBUGF("ERROR: Failed to create an encoder: %s",
                   opus_strerror(err));
        }
        assert(err == 0);
    }

    // Open audio device
    audio_info_t *audio_info;

    if ((err = audio_new(params->pcm_name, SND_PCM_STREAM_CAPTURE, 0,
                         FORMAT, CHANNELS, RATE_IN_HZ, SAMPLE_SIZE_IN_BYTES,
                         PERIOD_SIZE_IN_FRAMES, BUFFER_MULTIPLICATOR,
                         &audio_info)) < 0) {
        DEBUGF("audio_new: Could not initialize audio: %s", snd_strerror(err));
        int retval = AUDIO_ERROR;
        thread_exit(&retval);
    }
    audio_print_parameters(audio_info, "sender");
    assert(PERIOD_SIZE_IN_FRAMES == audio_info->period_size_in_frames);

    DEBUGF("Period size is %d bytes (%fms)", PERIOD_SIZE_IN_BYTES,
           PERIOD_SIZE_IN_MS);

    // Allocate memory for UDP buffer
    ssize_t udp_max_buf_size;
    if (params->opus_enabled) {
        udp_max_buf_size = HEADER_SIZE + OPUS_MAX_PACKET_LEN_IN_BYTES;
    } else {
        udp_max_buf_size = HEADER_SIZE + PERIOD_SIZE_IN_BYTES;
    }
    uint8_t *udp_buf = malloc(udp_max_buf_size);

    // Add userid to UDP buffer header
    uint32_t userid_nl = htonl(params->userid);
    memcpy(udp_buf, &userid_nl, sizeof(uint32_t));

    // Create a period buffer
    uint8_t period_buf[PERIOD_SIZE_IN_BYTES];

    // Let sequence number start with 1 (zero is reserved)
    uint32_t seqnum = 1;

    // Read from audio device and write to socket
    DEBUGP("Sending audio...");
    while (!kill_network_sender) {
        // Add timestamp to UDP buffer header
        uint64_t timestamp_nll = htonll(utimestamp());
        memcpy(&udp_buf[4], &timestamp_nll, sizeof(uint64_t));

        // Add seqnum to UDP buffer header
        uint32_t seqnum_nl = htonl(seqnum);
        memcpy(&udp_buf[12], &seqnum_nl, sizeof(uint32_t));

        // Add audio packet to UDP buffer
        uint16_t packet_len;
        if (params->opus_enabled) {
            snd_pcm_uframes_t frames;
            if ((frames = audio_read(audio_info, period_buf,
                                     PERIOD_SIZE_IN_FRAMES)) < 0) {
                continue;
            }
            assert(frames == PERIOD_SIZE_IN_FRAMES);
            if ((packet_len = opus_encode(opus_encoder,
                                          (opus_int16 *)period_buf,
                                          frames, &udp_buf[HEADER_SIZE],
                                          OPUS_MAX_PACKET_LEN_IN_BYTES)) < 0) {
                DEBUGF("Failed to Opus encode: %s", opus_strerror(packet_len));
                break;
            }
        } else {
            snd_pcm_uframes_t frames;
            if ((frames = audio_read(audio_info, &udp_buf[HEADER_SIZE],
                                     PERIOD_SIZE_IN_FRAMES)) < 0) {
                continue;
            }
            assert(frames == PERIOD_SIZE_IN_FRAMES);
            packet_len = PERIOD_SIZE_IN_BYTES;
        }

        // Add packet length to UDP buffer header

        uint16_t packet_len_ns = htons(packet_len);
        memcpy(&udp_buf[16], &packet_len_ns, sizeof(uint16_t));

        // Write to non-blocking socket
        ssize_t udp_buf_size = HEADER_SIZE + packet_len;
        for (int i = 0; i < params->naddr_ports; i++) {
            ssize_t n = sendto(sockfd, udp_buf, udp_buf_size, 0,
                               (struct sockaddr *)&addrs[i], sizeof(addrs[i]));
            if (n < 0) {
#ifdef DEBUG
                perror("sendto: Failed to write to socket");
#endif
            } else if (n != udp_buf_size) {
                DEBUGP("Too few bytes written to socket!");
            }
        }

        seqnum++;
    }

    DEBUGP("network_sender is shutting down!!!");
    audio_free(audio_info);
    free(udp_buf);
    close(sockfd);
    int retval = NETWORK_SENDER_DIED;
    thread_exit(&retval);
    return NULL;
}
