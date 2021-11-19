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

#define FOUR_SECONDS_IN_US (4 * 1000000)
#define DRAIN_BUF_SIZE 1

extern jb_table_t *jb_table;

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
        perror("socket: Socket creation failed");
        exit(SOCKET_ERROR);
    }

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(params->addr_port->port);

    if (bind(sockfd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perror("bind: Binding of socket failed");
        exit(SOCKET_ERROR);
    }

    struct timeval zero_timeout = {.tv_usec = 0, .tv_sec = 0};
    struct timeval one_second_timeout = {.tv_usec = 0, .tv_sec = 1};
    uint32_t udp_buf_size = HEADER_SIZE + PAYLOAD_SIZE_IN_BYTES;
    uint8_t drain_buf[DRAIN_BUF_SIZE];

    printf("Jitter buffer contains %dms of audio data (%d periods, %d bytes)\n",
           JITTER_BUFFER_SIZE_IN_MS,
           PERIODS_IN_JITTER_BUFFER,
           JITTER_BUFFER_SIZE_IN_BYTES);

    // Read from socket and write to jitter buffer
    while (true) {
        // Waiting for incoming audio
        printf("Waiting for incoming audio...\n");
        fd_set readfds;
        FD_ZERO(&readfds);
        FD_SET(sockfd, &readfds);
        if (select(sockfd + 1, &readfds, 0, 0, NULL) < 0) {
            perror("select: Failed to wait for incoming audio");
            break;
        }

        // Drain socket receive buffer
        while (true) {
            FD_ZERO(&readfds);
            FD_SET(sockfd, &readfds);
            struct timeval timeout = zero_timeout;
            int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
            if (nfds < 0) {
                perror("select: Failed to drain socket receive buffer\n");
                goto bail_out;
            } else if (nfds == 0) {
                // Socket receiver buffer has been drained!
                break;
            }
            if (recvfrom(sockfd, drain_buf, DRAIN_BUF_SIZE, 0, NULL,
                         NULL) < 0) {
                perror("recvfrom: Failed to drain socket receive buffer\n");
                goto bail_out;
            }
            FD_ZERO(&readfds);
            FD_SET(sockfd, &readfds);
        }
        printf("Socket receive buffer has been drained\n");

        uint64_t userid = 0;
        jb_t *jb = NULL;
        double latency = 0;
        uint64_t last_latency_printout = 0;

        // Read from socket and write to jitter buffer
        printf("Receiving audio...\n");
        while (true) {
            // Wait for incoming socket data (or timeout)
            FD_ZERO(&readfds);
            FD_SET(sockfd, &readfds);
            struct timeval timeout = one_second_timeout;
            int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
            if (nfds < 0) {
                perror("select: Failed to wait for incoming socket data");
                goto bail_out;
            } else if (nfds == 0) {
                // Timeout
                printf("No longer receiving audio!\n");
                break;
            }

            // Peek into socket and extract userid
            uint32_t new_userid;
            int n;
            if ((n = recvfrom(sockfd, &new_userid, sizeof(uint32_t), MSG_PEEK,
                              NULL, NULL)) < 0) {
                perror("recvfrom: Failed to peek into socket and extract \
userid");
                goto bail_out;
            } else if (n != sizeof(uint32_t)) {
                printf("Ignored truncated UDP packet!\n");
                break;
            }

            // Get jitter buffer
            if (userid != new_userid) {
                jb_table_take_rdlock(jb_table);
                if ((jb = jb_table_find(jb_table, new_userid)) == NULL) {
                    jb = jb_new(new_userid, params->opus_enabled);
                    jb_table_upgrade_to_wrlock(jb_table);
                    assert(jb_table_add(jb_table, jb) == JB_TABLE_SUCCESS);
                    jb_table_downgrade_to_rdlock(jb_table);
                }
                jb_table_release_lock(jb_table);
                userid = new_userid;
            }

            // Jitter buffer is exhausted according to audio sink
            if (jb->exhausted) {
                jb_take_wrlock(jb);
                jb_free(jb, true);
                jb_release_lock(jb);
            }

            // Prepare new jitter buffer entry
            jb_entry_t *jb_entry;
            if (jb->nentries > PERIODS_IN_JITTER_BUFFER) {
                jb_take_wrlock(jb);
                jb_entry = jb_pop(jb);
                jb_release_lock(jb);
            } else {
                jb_entry = jb_entry_new(udp_buf_size);
            }

            // Read from socket
            if ((n = recvfrom(sockfd, jb_entry->data, udp_buf_size, 0, NULL,
                              NULL)) < 0) {
                perror("recvfrom: Failed to read from socket");
                goto bail_out;
            } else if (n != udp_buf_size) {
                printf("Ignored truncated UDP packet!\n");
                break;
            }

            // Calculate latency (for developement debugging only)
            uint64_t timestamp;
            memcpy(&timestamp, &jb_entry->data[4], sizeof(uint64_t));
            uint64_t current_timestamp = utimestamp();
            // NOTE: Disable to allow development machines without NTP client
            //assert(current_timestamp > timestamp);
            latency = latency * 0.9 + (current_timestamp - timestamp) * 0.1;
            if (current_timestamp - last_latency_printout >
                FOUR_SECONDS_IN_US) {
                // NOTE: Disable to remove noise on stdout
                //printf("Latency: %fms\n", latency / 1000);
                last_latency_printout = current_timestamp;
            }

            // Add seqnum to jitter buffer entry and insert entry
            uint32_t seqnum;
            memcpy(&seqnum, &jb_entry->data[12], sizeof(seqnum));
            if (jb->nentries > 0) {
                if (jb->tail->seqnum == seqnum) {
                    // NOTE: Disable to remove noise on stdout
                    printf("Duplicated UDP packet (%d)\n", seqnum);
                } else if (jb->tail->seqnum + 1 != seqnum) {
                    // NOTE: Disable to remove noise on stdout
                    printf("Missing UDP packet %d. Got %d instead.\n",
                           jb->tail->seqnum + 1, seqnum);
                }
            }
            jb_entry->seqnum = seqnum;

            // Peak value/average handling
            uint16_t peak_value = 0;
            int16_t *s16data = (int16_t *)&jb_entry->data[HEADER_SIZE];
            for (uint16_t i = 0; i < PERIOD_SIZE_IN_FRAMES * CHANNELS; i++) {
                uint16_t udata = s16data[i] + 32768;
                if (udata > peak_value) {
                    peak_value = udata;
                }
            }
            jb->peak_values[jb->peak_index] = peak_value;
            if (++jb->peak_index % jb->npeak_values == 0) {
                jb->peak_index = 0;
                jb->peak_average =
                    root_mean_square(jb->peak_values, jb->npeak_values);
                jb_table_take_wrlock(jb_table);
                jb_table_sort(jb_table);
                jb_table_release_lock(jb_table);
            }

            // Insert buffer entry
            jb_take_wrlock(jb);
            assert(jb_insert(jb, jb_entry) != 0);
            jb_release_lock(jb);
        }

        printf("Erase all jitter buffers\n");
        jb_table_take_wrlock(jb_table);
        jb_table_free(jb_table, true);
        jb_table_release_lock(jb_table);
    }

 bail_out:
    fprintf(stderr, "network_receiver is shutting down!!!\n");
    close(sockfd);
    exit(NETWORK_RECEIVER_DIED);
}
