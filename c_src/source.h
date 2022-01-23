#ifndef _SOURCE_H_
#define _SOURCE_H_

#include <stdbool.h>
#include <inttypes.h>
#include "uthash/uthash.h"

typedef struct {
    uint32_t id;
    int sockfd;
    uint16_t port;
    bool used;
    UT_hash_handle hh;
} source_t;

source_t *source_new(void);
void source_free(source_t *source);

#endif
