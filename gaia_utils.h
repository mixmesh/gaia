#ifndef _GAIA_UTILS_H_
#define _GAIA_UTILS_H_

#include <pthread.h>
#include <arpa/inet.h>
#include <inttypes.h>

int string_to_long(char *string, long *value);
int get_addr_port(char *arg, in_addr_t *addr, uint16_t *port);
int set_fifo_scheduling(pthread_attr_t *attr, int8_t priority_offset);

#endif
