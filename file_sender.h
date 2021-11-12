#ifndef _FILE_SENDER_H_
#define _FILE_SENDER_H_

#include <arpa/inet.h>
#include <inttypes.h>

typedef struct {
    in_addr_t addr;
    uint16_t port;
} file_sender_addr_port_t;

typedef struct {
    uint32_t userid;
    char *filename;
    uint8_t naddr_ports;
    file_sender_addr_port_t *addr_ports;  
} file_sender_params_t;

void *file_sender(void *arg);

#endif
