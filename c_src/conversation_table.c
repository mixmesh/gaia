#include <assert.h>
#include "conversation_table.h"

conversation_table_t *conversation_table_new(void) {
    conversation_table_t
        *conversation_table = malloc(sizeof(conversation_table_t));;
    conversation_table->conversation = NULL;
    conversation_table->mutex = malloc(sizeof(thread_mutex_t));
    assert(thread_mutex_init(conversation_table->mutex,
                             "conversation_table") == 0);
    return conversation_table;
}

void conversation_table_free(conversation_table_t *conversation_table) {
    conversation_t *conversation, *tmp;
    HASH_ITER(hh, conversation_table->conversation, conversation, tmp) {
        conversation_free(conversation);
    }
    assert(thread_mutex_destroy(conversation_table->mutex) == 0);
    free(conversation_table);
}

void conversation_table_add(conversation_table_t *conversation_table,
                            conversation_t *conversation) {
    HASH_ADD_UINT32(conversation_table->conversation, id, conversation);
}

conversation_t *conversation_table_find(
                    conversation_table_t *conversation_table, uint32_t id) {
    conversation_t *conversation;
    HASH_FIND_UINT32(conversation_table->conversation, &id, conversation);
    return conversation;
}

void conversation_table_delete(conversation_table_t *conversation_table,
                               conversation_t *conversation) {
    HASH_DEL(conversation_table->conversation, conversation);
    conversation_free(conversation);
}

uint16_t conversation_table_count(conversation_table_t *conversation_table) {
    return HASH_COUNT(conversation_table->conversation);
}

void conversation_table_foreach(conversation_table_t *conversation_table,
                          void (*callback)(conversation_t *)) {
    conversation_t *conversation, *tmp;
    HASH_ITER(hh, conversation_table->conversation, conversation, tmp) {
        callback(conversation);
    }
}

void conversation_table_take_mutex(conversation_table_t *conversation_table) {
    assert(thread_mutex_lock(conversation_table->mutex) == 0);
}

void conversation_table_release_mutex(
         conversation_table_t *conversation_table) {
    assert(thread_mutex_unlock(conversation_table->mutex) == 0);
}
