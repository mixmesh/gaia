#include <stdio.h>
#include <stdbool.h>
#include <opus/opus.h>
#include "globals.h"
#include "timing.h"
#include "file_sender.h"
#include "audio.h"
#include "globals.h"

#define FILE_CACHE_SIZE (PERIOD_SIZE_IN_BYTES * 100)

void *file_sender(void *arg) {
    int err;

    // Parameters
    file_sender_params_t *params = (file_sender_params_t *)arg;

    // Open file
    FILE *fd;
    if ((fd = fopen(params->filename, "r")) == NULL) {
        perror("fopen: Could not open filename");
        exit(FILE_ERROR);
    }

    // Check file size
    fseek(fd, 0L, SEEK_END);
    if (ftell(fd) < FILE_CACHE_SIZE) {
        fprintf(stderr, "%s is smaller than %d bytes\n", params->filename,
                FILE_CACHE_SIZE);
        fclose(fd);
        exit(FILE_ERROR);
    }
    rewind(fd);

    // Create socket
    int sockfd = -1;
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket: Socket creation failed");
        fclose(fd);
        exit(SOCKET_ERROR);
    }

    // Make socket non-blocking
    int flags = fcntl(sockfd, F_GETFL, 0);
    if (flags < 0) {
        perror("fcntl: Socket could not be made non-blocking");
        fclose(fd);
        close(sockfd);
        exit(SOCKET_ERROR);
    }
    if (fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) < 0) {
        perror("fcntl: Socket could not be made non-blocking");
        fclose(fd);
        close(sockfd);
        exit(SOCKET_ERROR);
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
            fprintf(stderr, "opus_encoder_create: %s\n", opus_strerror(err));
        }
        assert(err == 0);
    }

    // Allocate memory for UDP buffer
    ssize_t udp_max_buf_size;
    if (params->opus_enabled) {
        udp_max_buf_size = HEADER_SIZE + OPUS_MAX_PACKET_LEN_IN_BYTES;
    } else {
        udp_max_buf_size = HEADER_SIZE + PERIOD_SIZE_IN_BYTES;
    }
    uint8_t *udp_buf = malloc(udp_max_buf_size);

    // Add gaia-id to UDP buffer header
    uint32_t gaia_id_nl = htonl(params->gaia_id);
    memcpy(udp_buf, &gaia_id_nl, sizeof(uint32_t));

    // Let sequence number start with 1 (zero is reserved)
    uint32_t seqnum = 1;

    struct timespec period_size_as_tsp =
        {
         .tv_sec = 0,
         .tv_nsec = PERIOD_SIZE_IN_NS
        };
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);

    char file_cache[FILE_CACHE_SIZE];
    uint32_t file_cache_index = FILE_CACHE_SIZE;

    printf("Period size is %d bytes (%d frames, %ldns)\n",
           PERIOD_SIZE_IN_BYTES, PERIOD_SIZE_IN_FRAMES,
           period_size_as_tsp.tv_nsec);

    // Read from file and write to socket
    printf("Sending audio...\n");
    while (true) {
        // Add timestamp to UDP buffer header
        uint64_t timestamp_nll = htonll(utimestamp());
        memcpy(&udp_buf[4], &timestamp_nll, sizeof(uint64_t));

        // Add seqnum to UDP buffer header
        uint32_t seqnum_nl = htonl(seqnum);
        memcpy(&udp_buf[12], &seqnum_nl, sizeof(uint32_t));

        // Cache file to RAM (if needed)
        if (file_cache_index == FILE_CACHE_SIZE) {
            size_t n = fread(file_cache, 1, FILE_CACHE_SIZE, fd);
            if (n < FILE_CACHE_SIZE) {
                if (feof(fd)) {
                    printf("Reached end of file in %s. Start from scratch!\n",
                           params->filename);
                    printf("Reached end of file. Start from scratch!\n");
                    rewind(fd);
                    uint32_t more_bytes = FILE_CACHE_SIZE - n;
                    if (fread(&file_cache[n], 1, more_bytes, fd) < more_bytes) {
                        perror("fread");
                        break;
                    }
                } else {
                    perror("fread");
                    break;
                }
            }
            file_cache_index = 0;
        }

        // Add cached audio packet to UDP buffer
        uint16_t packet_len;
        if (params->opus_enabled) {
            if ((packet_len = opus_encode(
                                  opus_encoder,
                                  (opus_int16 *)&file_cache[file_cache_index],
                                  PERIOD_SIZE_IN_FRAMES, &udp_buf[HEADER_SIZE],
                                  OPUS_MAX_PACKET_LEN_IN_BYTES)) < 0) {
                fprintf(stderr, "opus_encode: %s\n", opus_strerror(packet_len));
                break;
            }
        } else {
            memcpy(&udp_buf[HEADER_SIZE], &file_cache[file_cache_index],
                   PERIOD_SIZE_IN_BYTES);
            packet_len = PERIOD_SIZE_IN_BYTES;
        }

        // Add packet length to UDP buffer header
        uint16_t packet_len_ns = htons(packet_len);
        memcpy(&udp_buf[16], &packet_len_ns, sizeof(uint16_t));

        // Sleep (very carefully)
        struct timespec next_time;
        timespecadd(&time, &period_size_as_tsp, &next_time);
        struct timespec rem;
        assert(clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
                               &next_time, &rem) == 0);
        memcpy(&time, &next_time, sizeof(struct timespec));

        // Write to non-blocking socket
        ssize_t udp_buf_size = HEADER_SIZE + packet_len;
        for (int i = 0; i < params->naddr_ports; i++) {
            ssize_t n = sendto(sockfd, udp_buf, udp_buf_size, 0,
                               (struct sockaddr *)&addrs[i], sizeof(addrs[i]));
            if (n < 0) {
                perror("sendto: Failed to write to socket");
            } else if (n != udp_buf_size) {
                printf("sendto: Too few bytes written to socket!\n");
            }
        }

        file_cache_index += PERIOD_SIZE_IN_BYTES;
        seqnum++;
    }

    fprintf(stderr, "file_sender is shutting down!!!\n");
    free(udp_buf);
    close(sockfd);
    fclose(fd);
    exit(FILE_SENDER_DIED);
}
