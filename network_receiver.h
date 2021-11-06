#ifndef _NETWORK_RECEIVER_H_
#define _NETWORK_RECEIVER_H_

#include <arpa/inet.h>
#include <inttypes.h>
#include <stdbool.h>

typedef struct {
  in_addr_t addr;
  uint16_t port;
} network_receiver_params_t;

void *network_receiver(void *arg);

#endif
