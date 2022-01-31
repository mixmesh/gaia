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
#include "conversation_table.h"

#define WAIT_FOR_INCOMING_AUDIO_TIMEOUT 2000
#define FOUR_SECONDS_IN_US (4 * 1000000)
#define DRAIN_BUF_SIZE 1

extern jb_table_t *jb_table;
extern bool kill_network_receiver;
extern conversation_table_t *conversation_table;

int set_fds(fd_set *fds) {
    FD_ZERO(fds);
    int sockfd = -1;
    void set_fd(conversation_t *conversation) {
        FD_SET(conversation->sockfd, fds);
        sockfd = conversation->sockfd > sockfd ? conversation->sockfd : sockfd;
    }
    conversation_table_take_mutex(conversation_table);
    conversation_table_foreach(conversation_table, set_fd);
    conversation_table_release_mutex(conversation_table);
    return sockfd;
}

uint16_t root_mean_square(uint16_t *peak_values, uint16_t n) {
    double sum = 0.0;
    for(uint16_t i = 0; i < n; i++)
        sum += (double)peak_values[i] * peak_values[i];
    return sqrt(sum / n);
}

void *network_receiver(void *arg) {
    fd_set readfds;
    struct timeval zero_timeout = {.tv_usec = 0, .tv_sec = 0};
    struct timeval wait_for_incoming_audio_timeout =
        {.tv_usec = 0, .tv_sec = 2};

    ssize_t udp_max_buf_size =
        OPUS_MAX_PACKET_LEN_IN_BYTES > PERIOD_SIZE_IN_BYTES ?
        HEADER_SIZE + OPUS_MAX_PACKET_LEN_IN_BYTES :
        HEADER_SIZE + PERIOD_SIZE_IN_BYTES;

    INFOF("Jitter buffer contains %ums of audio data (%u periods, %u bytes)",
          JITTER_BUFFER_SIZE_IN_MS,
          PERIODS_IN_JITTER_BUFFER,
          JITTER_BUFFER_SIZE_IN_BYTES);

    // Read from sockets and write to jitter buffers
    while (!kill_network_receiver) {
        // Waiting for incoming audio
        DEBUGF("Waiting for incoming audio...");
        int sockfd = set_fds(&readfds);
        if (sockfd == -1) {
            msleep(WAIT_FOR_INCOMING_AUDIO_TIMEOUT);
            continue;
        }
        struct timeval timeout = wait_for_incoming_audio_timeout;
        int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
        if (nfds < 0) {
            perror("select: Failed to wait for incoming audio");
            break;
        } else if (nfds == 0) {
            continue;
        }
        INFOF("Incoming audio!");

        // Drain socket receive buffers
        void drain_socket_receive_buffer(conversation_t *conversation) {
            if (!FD_ISSET(conversation->sockfd, &readfds)) {
                return;
            }

            fd_set conversationfds;
            while (true) {
                FD_ZERO(&conversationfds);
                FD_SET(conversation->sockfd, &conversationfds);
                struct timeval timeout = zero_timeout;
                int nfds = select(conversation->sockfd + 1, &conversationfds,
                                  0, 0, &timeout);
                if (nfds < 0) {
                    perror("select: Failed to drain socket receive buffer\n");
                    return;
                } else if (nfds == 0) {
                    // Socket receiver buffer has been drained!
                    return;
                }
                uint8_t drain_buf[DRAIN_BUF_SIZE];
                if (recvfrom(conversation->sockfd, drain_buf, DRAIN_BUF_SIZE,
                             0, NULL, NULL) < 0) {
                    perror("recvfrom: Failed to drain socket receive buffer\n");
                    return;
                }
            }
        };
        INFOF("Drain socket receive buffers...");
        conversation_table_take_mutex(conversation_table);
        conversation_table_foreach(conversation_table,
                                   drain_socket_receive_buffer);
        conversation_table_release_mutex(conversation_table);
        INFOF("Socket receive buffers have been drained");

        INFOF("Erase all stale jitter buffers...");
        jb_table_free(jb_table, true);
        INFOF("Stale jitter buffers have been erased");

        uint32_t peer_id = 0;
        jb_t *jb = NULL;
        double latency = 0;
        uint64_t last_latency_printout = 0;

        void read_from_conversation(conversation_t *conversation) {
            if (!FD_ISSET(conversation->sockfd, &readfds)) {
                return;
            }

            DEBUGF("Read from conversation with peer id %d", conversation->id);

            // Peek into socket and extract buffer header
            uint8_t header_buf[HEADER_SIZE];
            struct sockaddr src_addr;
            socklen_t addrlen = sizeof(src_addr);
            int n;
            if ((n = recvfrom(conversation->sockfd, header_buf, HEADER_SIZE,
                              MSG_PEEK, &src_addr, &addrlen)) < 0) {
                perror("recvfrom: Failed to peek into socket and extract \
gaia-id");
                return;
            } else if (n != HEADER_SIZE) {
                ERRORF("recvfrom: Ignored truncated UDP packet!");
                return;
            }

            uint32_t new_peer_id = ntohl(*(uint32_t *)&header_buf[0]);
            uint16_t packet_len = ntohs(*(uint16_t *)&header_buf[16]);

            // Get jitter buffer
            if (peer_id != new_peer_id) {
                jb_table_take_rdlock(jb_table);
                if ((jb = jb_table_find(jb_table, new_peer_id)) == NULL) {
                    jb = jb_new(new_peer_id, true);
                    jb_table_upgrade_to_wrlock(jb_table);
                    assert(jb_table_add(jb_table, jb) == JB_TABLE_SUCCESS);
                    jb_table_downgrade_to_rdlock(jb_table);
                }
                jb_table_release_rdlock(jb_table);
                peer_id = new_peer_id;
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
            if ((n = recvfrom(conversation->sockfd, jb_entry->udp_buf,
                              udp_buf_size, 0, NULL, NULL)) < 0) {
                perror("recvfrom: Failed to read from socket");
                return;
            } else if (n != udp_buf_size) {
                ERRORF("recvfrom: Ignored truncated UDP packet!");
                return;
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
                DEBUGF("Latency: %fms", latency / 1000);
                last_latency_printout = current_timestamp;
            }

            // Add seqnum to jitter buffer entry and insert entry
            uint32_t seqnum_nl;
            memcpy(&seqnum_nl, &jb_entry->udp_buf[12], sizeof(uint32_t));
            uint32_t seqnum = ntohl(seqnum_nl);

            if (jb->nentries > 0) {
                if (jb->tail->seqnum == seqnum) {
                    INFOF("Duplicated UDP packet (%u)", seqnum);
                } else if (jb->tail->seqnum + 1 != seqnum) {
                    INFOF("Missing UDP packet %u. Got %u instead.",
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
        };

        // Read from sockets and write to jitter buffer
        INFOF("Receiving audio...");
        while (!kill_network_receiver) {
            // Wait for incoming socket data (or timeout)
            int sockfd = set_fds(&readfds);
            assert(sockfd != -1);
            struct timeval timeout = wait_for_incoming_audio_timeout;
            int nfds = select(sockfd + 1, &readfds, 0, 0, &timeout);
            if (nfds < 0) {
                perror("select: Failed to wait for incoming socket data");
                break;
            } else if (nfds == 0) {
                // Timeout
                INFOF("No longer receiving audio!\n");
                break;
            }

            conversation_table_take_mutex(conversation_table);
            conversation_table_foreach(conversation_table,
                                       read_from_conversation);
            conversation_table_release_mutex(conversation_table);
        }
    }

    INFOF("network_receiver is shutting down!!!");
    conversation_table_free(conversation_table);
    int retval = NETWORK_RECEIVER_DIED;
    thread_exit(&retval);
    return NULL;
}
