#ifndef _JB_TABLE_H_
#define _JB_TABLE_H_

#include <stdbool.h>
#include "jb.h"
#include "threadlib.h"
#include "uthash/uthash.h"

#define JB_TABLE_SUCCESS 0
#define JB_TABLE_ALREADY_EXISTS -1

#define HASH_FIND_UINT32(head, findint, out) \
    HASH_FIND(hh, head, findint, sizeof(uint32_t), out)
#define HASH_ADD_UINT32(head, intfield, add) \
    HASH_ADD(hh, head, intfield, sizeof(uint32_t), add)

typedef struct {
    jb_t *jb;
    thread_rwlock_t *rwlock;
    thread_mutex_t *lock_mutex;
} jb_table_t;

jb_table_t *jb_table_new(void);
void jb_table_free(jb_table_t *jb_table, bool free_entries_only);
int8_t jb_table_add(jb_table_t *jb_table, jb_t *jb);
jb_t *jb_table_find(jb_table_t *jb_table, uint32_t peer_id);
void jb_table_delete(jb_table_t *jb_table, uint32_t peer_id);
uint16_t jb_table_count(jb_table_t *jb_table);
void jb_table_foreach(jb_table_t *jb_table, void (*callback)(jb_t *t));
void jb_table_sort(jb_table_t *jb_table);
void jb_table_take_rdlock(jb_table_t *jb_table);
void jb_table_take_wrlock(jb_table_t *jb_table);
void jb_table_release_rdlock(jb_table_t *jb_table);
void jb_table_release_wrlock(jb_table_t *jb_table);
void jb_table_upgrade_to_wrlock(jb_table_t *jb_table);
void jb_table_downgrade_to_rdlock(jb_table_t *jb_table);

#endif
