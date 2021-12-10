#include <unistd.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/time.h>
#include <assert.h>
#include <math.h>
#include "timing.h"
#include "jb_table.h"
#include "network_receiver.h"
#include "globals.h"
#include "gaia_utils.h"

#define FOUR_SECONDS_IN_US (4 * 1000000)
#define DRAIN_BUF_SIZE 1

extern jb_table_t *jb_table;
extern bool kill_network_receiver;

uint16_t root_mean_square(uint16_t *peak_values, uint16_t n) {
    double sum = 0.0;
    for(uint16_t i = 0; i < n; i++)
        sum += (double)peak_values[i] * peak_values[i];
    return sqrt(sum / n);
}

void *network_receiver(void *arg) {
    int sockfd = -1;

    // Parameters
    network_receiver_params_t *params = (network_receiver_params_t *)arg;

    // Create and bind socket
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
#ifdef DEBUG
        perror("socket: Socket creation failed");
#endif
        int retval = SOCKET_ERROR;
        thread_exit(&retval);
    }

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(params->addr_port->port);

    if (bind(sockfd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
#ifdef DEBUG
        perror("bind: Binding of socket failed");
#endif
        int retval = SOCKET_ERROR;
        thread_exit(&retval);
    }

    struct timeval zero_timeout = {.tv_usec = 0, .tv_sec = 0};
    struct timeval one_second_timeout = {.tv_usec = 0, .tv_sec = 1};
    struct timeval two_second_timeout = {.tv_usec = 0, .tv_sec = 2};

    ssize_t udp_max_buf_size;
    if (params->opus_enabled) {
        udp_max_buf_size = HEADER_SIZE + OPUS_MAX_PACKET_LEN_IN_BYTES;
    } else {
        udp_max_buf_size = HEADER_SIZE + PERIOD_SIZE_IN_BYTES;
    }

    uint8_t drain_buf[DRAIN_BUF_SIZE];

    DEBUGF("Jitter buffer contains %dms of audio data (%d periods, %d bytes)",
           JITTER_BUFFER_SIZE_IN_MS,
           PERIODS_IN_JITTER_BUFFER,
           JITTER_BUFFER_SIZE_IN_BYTES);

    // Read from socket and write to jitter buffer
    while (!kill_network_receiver) {
        // Waiting for incoming audio
        DEBUGP("Waiting for incoming audio...");
        fd_set readfds;
        FD_ZERO(&readfds);
        FD_SET(sockfd, &readfds);
        struct timeval timeout = two_second_timeout;
        int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
        if (nfds < 0) {
#ifdef DEBUG
            perror("select: Failed to wait for incoming audio");
#endif
            break;
        } else if (nfds == 0) {
            continue;
        }

        // Drain socket receive buffer
        while (true) {
            FD_ZERO(&readfds);
            FD_SET(sockfd, &readfds);
            struct timeval timeout = zero_timeout;
            int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
            if (nfds < 0) {
#ifdef DEBUG
                perror("select: Failed to drain socket receive buffer\n");
#endif
                goto bail_out;
            } else if (nfds == 0) {
                // Socket receiver buffer has been drained!
                break;
            }
            if (recvfrom(sockfd, drain_buf, DRAIN_BUF_SIZE, 0, NULL,
                         NULL) < 0) {
#ifdef DEBUG
                perror("recvfrom: Failed to drain socket receive buffer\n");
#endif
                goto bail_out;
            }
            FD_ZERO(&readfds);
            FD_SET(sockfd, &readfds);
        }
        DEBUGP("Socket receive buffer has been drained");

        DEBUGP("Erase all stale jitter buffers");
        jb_table_free(jb_table, true);

        uint64_t gaia_id = 0;
        jb_t *jb = NULL;
        double latency = 0;
        uint64_t last_latency_printout = 0;

        // Read from socket and write to jitter buffer
        DEBUGP("Receiving audio...");
        while (!kill_network_receiver) {
            // Wait for incoming socket data (or timeout)
            FD_ZERO(&readfds);
            FD_SET(sockfd, &readfds);
            struct timeval timeout = one_second_timeout;
            int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
            if (nfds < 0) {
#ifdef DEBUG
                perror("select: Failed to wait for incoming socket data");
#endif
                goto bail_out;
            } else if (nfds == 0) {
                // Timeout
                DEBUGP("No longer receiving audio!\n");
                break;
            }

            // Peek into socket and extract buffer header
            uint8_t header_buf[HEADER_SIZE];
            int n;
            if ((n = recvfrom(sockfd, header_buf, HEADER_SIZE, MSG_PEEK, NULL,
                              NULL)) < 0) {
#ifdef DEBUG
                perror("recvfrom: Failed to peek into socket and extract \
gaia-id");
#endif
                goto bail_out;
            } else if (n != HEADER_SIZE) {
                DEBUGP("recvfrom: Ignored truncated UDP packet!");
                break;
            }
            uint32_t new_gaia_id = ntohl(*(uint32_t *)&header_buf[0]);
            uint16_t packet_len = ntohs(*(uint16_t *)&header_buf[16]);

            // Get jitter buffer
            if (gaia_id != new_gaia_id) {
                jb_table_take_rdlock(jb_table);
                if ((jb = jb_table_find(jb_table, new_gaia_id)) == NULL) {
                    jb = jb_new(new_gaia_id, params->opus_enabled);
                    jb_table_upgrade_to_wrlock(jb_table);
                    assert(jb_table_add(jb_table, jb) == JB_TABLE_SUCCESS);
                    jb_table_downgrade_to_rdlock(jb_table);
                }
                jb_table_release_rdlock(jb_table);
                gaia_id = new_gaia_id;
            }

            // Jitter buffer is exhausted according to audio sink
            if (jb->exhausted) {
                jb_free(jb, true);
            }

            // Prepare new jitter buffer entry
            jb_entry_t *jb_entry;
            if (jb->nentries > PERIODS_IN_JITTER_BUFFER) {
                jb_take_wrlock(jb);
                jb_entry = jb_pop(jb);
                jb_release_wrlock(jb);
            } else {
                jb_entry = jb_entry_new(udp_max_buf_size, PERIOD_SIZE_IN_BYTES);
            }

            // Read from socket
            ssize_t udp_buf_size = HEADER_SIZE + packet_len;
            if ((n = recvfrom(sockfd, jb_entry->udp_buf, udp_buf_size, 0, NULL,
                              NULL)) < 0) {
#ifdef DEBUG
                perror("recvfrom: Failed to read from socket");
#endif
                goto bail_out;
            } else if (n != udp_buf_size) {
                DEBUGP("recvfrom: Ignored truncated UDP packet!");
                break;
            }

            // Calculate latency (for developement debugging only)
            uint64_t timestamp_nll;
            memcpy(&timestamp_nll, &jb_entry->udp_buf[4], sizeof(uint64_t));
            uint64_t timestamp = ntohll(timestamp_nll);
            uint64_t current_timestamp = utimestamp();
            //assert(current_timestamp > timestamp);
            // NOTE: Disable if NTP isn't installed on test clients
            latency = latency * 0.9 + (current_timestamp - timestamp) * 0.1;
            if (current_timestamp - last_latency_printout >
                FOUR_SECONDS_IN_US) {
                //DEBUGF("Latency: %fms", latency / 1000);
                last_latency_printout = current_timestamp;
            }

            // Add seqnum to jitter buffer entry and insert entry
            uint32_t seqnum_nl;
            memcpy(&seqnum_nl, &jb_entry->udp_buf[12], sizeof(uint32_t));
            uint32_t seqnum = ntohl(seqnum_nl);

            if (jb->nentries > 0) {
                if (jb->tail->seqnum == seqnum) {
                    DEBUGF("Duplicated UDP packet (%d)", seqnum);
                } else if (jb->tail->seqnum + 1 != seqnum) {
                    DEBUGF("Missing UDP packet %d. Got %d instead.",
                           jb->tail->seqnum + 1, seqnum);
                }
            }
            jb_entry->seqnum = seqnum;

            // Peak value/average handling
            uint16_t peak_value = 0;
            int16_t *packet = (int16_t *)&jb_entry->udp_buf[HEADER_SIZE];
            for (uint16_t i = 0; i < PERIOD_SIZE_IN_FRAMES * CHANNELS; i++) {
                uint16_t next_value = packet[i] + 32768;
                if (next_value > peak_value) {
                    peak_value = next_value;
                }
            }
            jb->peak_values[jb->peak_index] = peak_value;
            if (++jb->peak_index % jb->npeak_values == 0) {
                jb->peak_index = 0;
                jb->peak_average =
                    root_mean_square(jb->peak_values, jb->npeak_values);
                jb_table_take_wrlock(jb_table);
                jb_table_sort(jb_table);
                jb_table_release_wrlock(jb_table);
            }

            // Insert buffer entry
            jb_take_wrlock(jb);
            assert(jb_insert(jb, jb_entry) != 0);
            jb_release_wrlock(jb);
        }
    }

 bail_out:
    DEBUGP("network_receiver is shutting down!!!");
    close(sockfd);
    int retval = NETWORK_RECEIVER_DIED;
    thread_exit(&retval);
    return NULL;
}
