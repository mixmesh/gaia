#ifndef _FILE_SENDER_H_
#define _FILE_SENDER_H_

#include <stdbool.h>
#include <inttypes.h>
#include "gaia_utils.h"

typedef struct {
    uint32_t userid;
    char filename[128];
    uint8_t naddr_ports;
    addr_port_t *addr_ports;
    bool opus_enabled;
} file_sender_params_t;

void *file_sender(void *arg);

#endif
