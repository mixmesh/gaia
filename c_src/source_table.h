#ifndef _SOURCE_TABLE_H_
#define _SOURCE_TABLE_H_

#include "threadlib.h"
#include "source.h"
#include "uthash/uthash.h"

#define HASH_FIND_UINT32(head, findint, out)            \
    HASH_FIND(hh, head, findint, sizeof(uint32_t), out)
#define HASH_ADD_UINT32(head, intfield, add)            \
    HASH_ADD(hh, head, intfield, sizeof(uint32_t), add)

typedef struct {
    source_t *source;
    thread_mutex_t *mutex;
} source_table_t;

source_table_t *source_table_new(void);
void source_table_free(source_table_t *source_table);
void source_table_add(source_table_t *source_table, source_t *source);
source_t *source_table_find(source_table_t *source_table, uint32_t id);
void source_table_delete(source_table_t *source_table, source_t *source);
uint16_t source_table_count(source_table_t *source_table);
void source_table_foreach(source_table_t *source_table,
                          void (*callback)(source_t *source));
void source_table_take_mutex(source_table_t *source_table);
void source_table_release_mutex(source_table_t *source_table);

#endif
