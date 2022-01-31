#ifndef _CONVERSATION_H_
#define _CONVERSATION_H_

#include <stdbool.h>
#include <inttypes.h>
#include <netinet/in.h>
#include "uthash/uthash.h"

typedef struct {
    uint32_t id;
    int sockfd;
    in_addr_t ip_address;
    uint16_t port;
    bool used;
    UT_hash_handle hh;
} conversation_t;

conversation_t *conversation_new(void);
void conversation_free(conversation_t *conversation);

#endif
