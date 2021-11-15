#include <stdio.h>
#include <stdbool.h>
#include "globals.h"
#include "timing.h"
#include "file_sender.h"
#include "audio.h"
#include "globals.h"

#define FILE_BUF_SIZE (PAYLOAD_SIZE_IN_BYTES * 500)

void *file_sender(void *arg) {
    // Extract parameters
    file_sender_params_t *sender_params = (file_sender_params_t *)arg;
    uint32_t userid = sender_params->userid;
    char *filename = sender_params->filename;
    uint8_t naddr_ports = sender_params->naddr_ports;
    file_sender_addr_port_t *addr_ports = sender_params->addr_ports;

    // Open file
    FILE *fd;
    if ((fd = fopen(filename, "r")) == NULL) {
        perror("fopen: Could not open filename");
        exit(FILE_ERROR);
    }

    // Check file size
    fseek(fd, 0L, SEEK_END);
    if (ftell(fd) < FILE_BUF_SIZE) {
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
    struct sockaddr_in addrs[naddr_ports];
    for (int i = 0; i < naddr_ports; i++) {
        memset(&addrs[i], 0, sizeof(addrs[i]));
        addrs[i].sin_family = AF_INET;
        addrs[i].sin_addr.s_addr = addr_ports[i].addr;
        addrs[i].sin_port = htons(addr_ports[i].port);
    }

    uint32_t udp_buf_size = HEADER_SIZE + PAYLOAD_SIZE_IN_BYTES;
    uint8_t *udp_buf = malloc(udp_buf_size);

    // Add userid to buffer header
    memcpy(udp_buf, &userid, sizeof(userid));

    // Let sequence number start with 1 (zero is reserved)
    uint32_t seqnum = 1;

    struct timespec period_size_as_tsp =
        {
         .tv_sec = 0,
         .tv_nsec = PERIOD_SIZE_IN_NS
        };
    struct timespec before_caching;

    char file_buf[FILE_BUF_SIZE];
    uint32_t file_buf_index = FILE_BUF_SIZE;

    // Read from file and write to socket
    printf("Sending audio...\n");
    while (true) {
        // Add timestamp to buffer header
        uint64_t timestamp = utimestamp();
        memcpy(&udp_buf[4], &timestamp, sizeof(timestamp));

        // Add seqnum to buffer header
        memcpy(&udp_buf[12], &seqnum, sizeof(seqnum));
        seqnum++;

        clock_gettime(CLOCK_MONOTONIC, &before_caching);

        // Cache file to RAM (if needed)
        if (file_buf_index == FILE_BUF_SIZE) {
            size_t read_bytes = fread(file_buf, 1, FILE_BUF_SIZE, fd);
            if (read_bytes < FILE_BUF_SIZE) {
                if (feof(fd)) {
                    printf("Reached end of file. Start from scratch!\n");
                    rewind(fd);
                    uint32_t more_bytes = FILE_BUF_SIZE - read_bytes;
                    if (fread(&file_buf[read_bytes], 1, more_bytes,
                              fd) < more_bytes) {
                        perror("fread");
                        break;
                    }
                } else {
                    perror("fread");
                    break;
                }
            }
            file_buf_index = 0;
        }

        // Insert payload from data cache into buffer
        memcpy(&udp_buf[HEADER_SIZE], &file_buf[file_buf_index],
               PAYLOAD_SIZE_IN_BYTES);
        file_buf_index += PAYLOAD_SIZE_IN_BYTES;

        // Sleep (very carefully)
        struct timespec now;
        clock_gettime(CLOCK_REALTIME, &now);
        struct timespec time_spent;
        timespecsub(&now, &before_caching, &time_spent);
        struct timespec delay;
        timespecsub(&period_size_as_tsp, &time_spent, &delay);
        struct timespec sleep_until;
        timespecadd(&now, &delay, &sleep_until);
        struct timespec rem;
        assert(clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
                               &sleep_until, &rem) == 0);

        // Write to non-blocking socket
        for (int i = 0; i < naddr_ports; i++) {
            ssize_t n  = sendto(sockfd, udp_buf, udp_buf_size, 0,
                                (struct sockaddr *)&addrs[i],
                                sizeof(addrs[i]));
            if (n < 0) {
                perror("sendto: Failed to write to socket");
            } else if (n != udp_buf_size) {
                printf("Too few bytes written to socket!\n");
            }
        }
    }

    fprintf(stderr, "file_sender is shutting down!!!\n");
    free(udp_buf);
    close(sockfd);
    fclose(fd);
    exit(FILE_SENDER_DIED);
}
