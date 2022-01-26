#include <assert.h>
#include "source_table.h"

source_table_t *source_table_new(void) {
    source_table_t *source_table = malloc(sizeof(source_table_t));;
    source_table->source = NULL;
    source_table->mutex = malloc(sizeof(thread_mutex_t));
    assert(thread_mutex_init(source_table->mutex, "source_table") == 0);
    return source_table;
}

void source_table_free(source_table_t *source_table) {
    source_t *source, *tmp;
    HASH_ITER(hh, source_table->source, source, tmp) {
        source_free(source);
    }
    assert(thread_mutex_destroy(source_table->mutex) == 0);
    free(source_table);
}

void source_table_add(source_table_t *source_table, source_t *source) {
    HASH_ADD_UINT32(source_table->source, id, source);
}

source_t *source_table_find(source_table_t *source_table, uint32_t id) {
    source_t *source;
    HASH_FIND_UINT32(source_table->source, &id, source);
    return source;
}

void source_table_delete(source_table_t *source_table, source_t *source) {
    HASH_DEL(source_table->source, source);
    source_free(source);
}

uint16_t source_table_count(source_table_t *source_table) {
    return HASH_COUNT(source_table->source);
}

void source_table_foreach(source_table_t *source_table,
                          void (*callback)(source_t *)) {
    source_t *source, *tmp;
    HASH_ITER(hh, source_table->source, source, tmp) {
        callback(source);
    }
}

void source_table_take_mutex(source_table_t *source_table) {
    assert(thread_mutex_lock(source_table->mutex) == 0);
}

void source_table_release_mutex(source_table_t *source_table) {
    assert(thread_mutex_unlock(source_table->mutex) == 0);
}
