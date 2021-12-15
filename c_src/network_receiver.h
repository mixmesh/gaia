#ifndef _NETWORK_RECEIVER_H_
#define _NETWORK_RECEIVER_H_

#include <stdbool.h>
#include "gaia_utils.h"

typedef struct {
    port_t port;
} network_receiver_params_t;

void *network_receiver(void *arg);

#endif
