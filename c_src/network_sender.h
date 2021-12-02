#ifndef _NETWORK_SENDER_H_
#define _NETWORK_SENDER_H_

#include <inttypes.h>
#include <stdbool.h>
#include <opus/opus.h>
#include "gaia_utils.h"

typedef struct {
    char* pcm_name;
    uint32_t gaia_id;
    uint8_t naddr_ports;
    addr_port_t *addr_ports;
    bool opus_enabled;
} network_sender_params_t;

void *network_sender(void *arg);

#endif
