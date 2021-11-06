#ifndef _NETWORK_SENDER_H_
#define _NETWORK_SENDER_H_

#include <arpa/inet.h>

typedef struct {
  uint32_t userid;
  in_addr_t addr;
  uint16_t port;
} network_sender_params_t;

void *network_sender(void *arg);

#endif
