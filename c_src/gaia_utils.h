#ifndef _GAIA_UTILS_H_
#define _GAIA_UTILS_H_

#include <pthread.h>
#include <arpa/inet.h>
#include <inttypes.h>

#ifdef DEBUG
#ifdef LOG_TO_FILE
#define DEBUGF(f, ...) fprintf(LOG_FD, f "\r\n", ##__VA_ARGS__); fflush(LOG_FD)
#else
#define DEBUGF(f, ...) fprintf(LOG_FD, f "\r\n", ##__VA_ARGS__)
#endif
#else
#define DEBUGF(f, ...)
#endif

#ifdef INFO
#ifdef LOG_TO_FILE
#define INFOF(f, ...) fprintf(LOG_FD, f "\r\n", ##__VA_ARGS__); fflush(LOG_FD)
#define INFOFNNL(f, ...) fprintf(LOG_FD, f, ##__VA_ARGS__); fflush(LOG_FD)
#else
#define INFOF(f, ...) fprintf(LOG_FD, f "\r\n", ##__VA_ARGS__)
#define INFOFNNL(f, ...) fprintf(LOG_FD, f, ##__VA_ARGS__)
#endif
#else
#define INFOF(f, ...)
#endif

#ifdef LOG_TO_FILE
#define ERRORF(f, ...) fprintf(LOG_FD, f "\r\n", ##__VA_ARGS__); fflush(LOG_FD)
#else
#define ERRORF(f, ...) fprintf(LOG_FD, f "\r\n", ##__VA_ARGS__)
#endif

#if __BIG_ENDIAN__
#define htonll(x) (x)
#define ntohll(x) (x)
#else
#define htonll(x) (((uint64_t)htonl((x) & 0xFFFFFFFF) << 32) | htonl((x) >> 32))
#define ntohll(x) (((uint64_t)ntohl((x) & 0xFFFFFFFF) << 32) | ntohl((x) >> 32))
#endif

typedef uint16_t port_t;

typedef struct {
    in_addr_t addr;
    port_t port;
} addr_port_t;

int string_to_long(char *string, long *value);
int get_port(char *string, port_t *port);
int get_addr_port(char *arg, in_addr_t *addr, port_t *port);
int set_fifo_scheduling(pthread_attr_t *attr, int8_t priority_offset);
int ip4_to_int(int first, int second, int third, int fourth);

#endif
