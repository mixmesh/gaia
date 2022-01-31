#ifndef _CONVERSATION_TABLE_H_
#define _CONVERSATION_TABLE_H_

#include "threadlib.h"
#include "conversation.h"
#include "uthash/uthash.h"

#define HASH_FIND_UINT32(head, findint, out)            \
    HASH_FIND(hh, head, findint, sizeof(uint32_t), out)
#define HASH_ADD_UINT32(head, intfield, add)            \
    HASH_ADD(hh, head, intfield, sizeof(uint32_t), add)

typedef struct {
    conversation_t *conversation;
    thread_mutex_t *mutex;
} conversation_table_t;

conversation_table_t *conversation_table_new(void);
void conversation_table_free(conversation_table_t *conversation_table);
void conversation_table_add(conversation_table_t *conversation_table,
                            conversation_t *conversation);
conversation_t *conversation_table_find(conversation_table_t *conversation_table,
                                        uint32_t id);
void conversation_table_delete(conversation_table_t *conversation_table,
                               conversation_t *conversation);
uint16_t conversation_table_count(conversation_table_t *conversation_table);
void conversation_table_foreach(conversation_table_t *conversation_table,
                                void (*callback)(conversation_t *conversation));
void conversation_table_take_mutex(conversation_table_t *conversation_table);
void conversation_table_release_mutex(conversation_table_t *conversation_table);

#endif
