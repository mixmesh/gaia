#ifndef _JB_H_
#define _JB_H_

#include <pthread.h>
#include "uthash/uthash.h"

typedef struct jb_entry {
  uint32_t seqnum;
  uint8_t *data;
  struct jb_entry *next;
  struct jb_entry *prev;
} jb_entry_t;

typedef struct {
  uint32_t userid;
  jb_entry_t *playback;
  uint32_t playback_seqnum;
  uint32_t entries;
  pthread_rwlock_t *rwlock;
  jb_entry_t *tail;
  jb_entry_t *head;
  UT_hash_handle hh;
} jb_t;

#define ALREADY_EXISTS        (1 << 0)
#define TAIL_INSERTED         (1 << 1)
#define HEAD_INSERTED         (1 << 2)
#define FIRST_INSERTED        (1 << 3)
#define INTERMEDIATE_INSERTED (1 << 4)

jb_t *jb_new(uint32_t userid);
void jb_free(jb_t *jb);
jb_entry_t *jb_pop(jb_t *jb);
uint8_t jb_insert(jb_t *jb, jb_entry_t *new_jb_entry);
jb_entry_t *jb_get_entry(jb_t *jb, uint32_t index);
uint32_t jb_get_index(jb_t *jb, jb_entry_t *jb_entry);
void jb_take_rdlock(jb_t *jb);
void jb_take_wrlock(jb_t *jb);
void jb_release_lock(jb_t *jb);

jb_entry_t *jb_entry_new(uint32_t data_size);
void jb_entry_free(jb_entry_t *jb_entry);

#endif
