#include <stdio.h>
#include <assert.h>
#include "uthash/uthash.h"
#include "jb_table.h"

#define HASH_FIND_UINT32(head, findint, out) \
    HASH_FIND(hh, head, findint, sizeof(uint32_t), out)
#define HASH_ADD_UINT32(head, intfield, add) \
    HASH_ADD(hh, head, intfield, sizeof(uint32_t), add)

jb_table_t *jb_table_new(void) {
    jb_table_t *jb_table = malloc(sizeof(jb_table_t));
    jb_table->jb = NULL;
    jb_table->rwlock = malloc(sizeof(pthread_rwlock_t));
    assert(pthread_rwlock_init(jb_table->rwlock, NULL) == 0);
    jb_table->lock_mutex = malloc(sizeof(pthread_mutex_t));
    assert(pthread_mutex_init(jb_table->lock_mutex, NULL) == 0);
    return jb_table;
}

void jb_table_free(jb_table_t *jb_table, bool free_list_only) {
    jb_t *jb, *tmp;
    HASH_ITER(hh, jb_table->jb, jb, tmp) {
        jb_take_wrlock(jb);
        jb_free(jb, false);
    }
    if (free_list_only) {
        jb_table->jb = NULL;
    } else {
        assert(pthread_rwlock_destroy(jb_table->rwlock) == 0);
        free(jb_table->rwlock);
        assert(pthread_mutex_destroy(jb_table->lock_mutex) == 0);
        free(jb_table->lock_mutex);
        free(jb_table);
    }
}

int8_t jb_table_add(jb_table_t *jb_table, jb_t *jb) {
    if (jb_table_find(jb_table, jb->userid) != NULL) {
        return JB_TABLE_ALREADY_EXISTS;
    }
    HASH_ADD_UINT32(jb_table->jb, userid, jb);
    return JB_TABLE_SUCCESS;
}

jb_t *jb_table_find(jb_table_t *jb_table, uint32_t userid) {
    jb_t *jb;
    HASH_FIND_UINT32(jb_table->jb, &userid, jb);
    return jb;
}

void jb_table_delete(jb_table_t *jb_table, uint32_t userid) {
    jb_t *jb;
    HASH_FIND_UINT32(jb_table->jb, &userid, jb);
    if (jb != NULL) {
        HASH_DEL(jb_table->jb, jb);
        jb_free(jb, false);
    }
}

uint16_t jb_table_count(jb_table_t *jb_table) {
    return HASH_COUNT(jb_table->jb);
}

void jb_table_foreach(jb_table_t *jb_table, void (*callback)(jb_t *t)) {
    jb_t *jb, *tmp;
    HASH_ITER(hh, jb_table->jb, jb, tmp) {
        callback(jb);
    }
}

void jb_table_sort(jb_table_t *jb_table) {
    int sort_on_peak_average(jb_t *jb1, jb_t *jb2) {
        return jb2->peak_average - jb1->peak_average;
    }
    HASH_SORT(jb_table->jb, sort_on_peak_average);
}

void jb_table_take_rdlock(jb_table_t *jb_table) {
    assert(pthread_mutex_lock(jb_table->lock_mutex) == 0);
    assert(pthread_rwlock_rdlock(jb_table->rwlock) == 0);
    assert(pthread_mutex_unlock(jb_table->lock_mutex) == 0);
}

void jb_table_take_wrlock(jb_table_t *jb_table) {
    assert(pthread_mutex_lock(jb_table->lock_mutex) == 0);
    assert(pthread_rwlock_wrlock(jb_table->rwlock) == 0);
    assert(pthread_mutex_unlock(jb_table->lock_mutex) == 0);
}

void jb_table_release_lock(jb_table_t *jb_table) {
    assert(pthread_rwlock_unlock(jb_table->rwlock) == 0);
}

void jb_table_upgrade_to_wrlock(jb_table_t *jb_table) {
    assert(pthread_mutex_lock(jb_table->lock_mutex) == 0);
    assert(pthread_rwlock_unlock(jb_table->rwlock) == 0);
    assert(pthread_rwlock_wrlock(jb_table->rwlock) == 0);
    assert(pthread_mutex_unlock(jb_table->lock_mutex) == 0);
}

void jb_table_downgrade_to_rdlock(jb_table_t *jb_table) {
    assert(pthread_mutex_lock(jb_table->lock_mutex) == 0);
    assert(pthread_rwlock_unlock(jb_table->rwlock) == 0);
    assert(pthread_rwlock_rdlock(jb_table->rwlock) == 0);
    assert(pthread_mutex_unlock(jb_table->lock_mutex) == 0);
}
