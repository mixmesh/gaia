#ifndef _GAIA_UTILS_H_
#define _GAIA_UTILS_H_

#include <pthread.h>
#include <arpa/inet.h>
#include <inttypes.h>

#ifdef DEBUG
#define DEBUGF(f, a...) fprintf(stderr, f "\n", a)
#define DEBUGP(s) fprintf(stderr, s "\n")
#else
#define DEBUGF(f, a...)
#define DEBUGP(s)
#endif

#if __BIG_ENDIAN__
#define htonll(x) (x)
#define ntohll(x) (x)
#else
#define htonll(x) (((uint64_t)htonl((x) & 0xFFFFFFFF) << 32) | htonl((x) >> 32))
#define ntohll(x) (((uint64_t)ntohl((x) & 0xFFFFFFFF) << 32) | ntohl((x) >> 32))
#endif

typedef struct {
    in_addr_t addr;
    uint16_t port;
} addr_port_t;

int string_to_long(char *string, long *value);
int get_addr_port(char *arg, in_addr_t *addr, uint16_t *port);
int set_fifo_scheduling(pthread_attr_t *attr, int8_t priority_offset);

#endif
