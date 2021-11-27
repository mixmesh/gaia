#ifndef _JB_H_
#define _JB_H_

#include <pthread.h>
#include <stdbool.h>
#include <opus/opus.h>
#include "uthash/uthash.h"

typedef struct jb_entry {
    uint32_t seqnum;
    uint8_t *udp_buf;
    uint8_t *period_buf;
    struct jb_entry *next;
    struct jb_entry *prev;
} jb_entry_t;

typedef struct {
    uint32_t userid;
    jb_entry_t *playback;
    uint32_t playback_seqnum;
    bool exhausted;
    uint32_t nentries;
    pthread_rwlock_t *rwlock;
    uint16_t npeak_values;
    uint16_t *peak_values;
    uint16_t peak_index;
    uint16_t peak_average;
    OpusDecoder *opus_decoder;
    jb_entry_t *tail;
    jb_entry_t *head;
    UT_hash_handle hh;
} jb_t;

#define ALREADY_EXISTS        (1 << 0)
#define TAIL_INSERTED         (1 << 1)
#define HEAD_INSERTED         (1 << 2)
#define FIRST_INSERTED        (1 << 3)
#define INTERMEDIATE_INSERTED (1 << 4)

jb_t *jb_new(uint32_t userid, bool opus_enabled);
void jb_free(jb_t *jb, bool free_entries_only);
jb_entry_t *jb_pop(jb_t *jb);
uint8_t jb_insert(jb_t *jb, jb_entry_t *new_jb_entry);
jb_entry_t *jb_get_entry(jb_t *jb, uint32_t index);
uint32_t jb_get_index(jb_t *jb, jb_entry_t *jb_entry);
void jb_take_rdlock(jb_t *jb);
void jb_take_wrlock(jb_t *jb);
void jb_release_lock(jb_t *jb);

jb_entry_t *jb_entry_new(uint16_t udp_buf_size, uint16_t period_buf_size);
void jb_entry_free(jb_entry_t *jb_entry);

#endif
