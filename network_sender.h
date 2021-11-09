#ifndef _NETWORK_SENDER_H_
#define _NETWORK_SENDER_H_

#include <arpa/inet.h>
#include <inttypes.h>

typedef struct {
  in_addr_t addr;
  uint16_t port;
} network_sender_addr_port_t;

typedef struct {
  uint32_t userid;
  uint8_t naddr_ports;
  network_sender_addr_port_t *addr_ports;  
} network_sender_params_t;

void *network_sender(void *arg);

#endif
