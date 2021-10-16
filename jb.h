#ifndef _JB_H_
#define _JB_H_

#include <stdint.h>
#include <inttypes.h>
#include "uthash/uthash.h"

typedef struct jb_entry {
  uint32_t index;
  uint8_t *data;
  struct jb_entry *next;
  struct jb_entry *prev;
} jb_entry_t;

typedef struct {
  char name[32];
  uint32_t entries;
  jb_entry_t *head;
  jb_entry_t *tail;
  UT_hash_handle hh;
} jb_t;

#define ALREADY_EXISTS        (1 << 0)
#define TAIL_INSERTED         (1 << 1)
#define HEAD_INSERTED         (1 << 2)
#define FIRST_INSERTED        (1 << 3)
#define INTERMEDIATE_INSERTED (1 << 4)

jb_t *jb_new(char *name);
void jb_free(jb_t *jb);
jb_entry_t *jb_pop(jb_t *jb);
uint8_t jb_insert(jb_t *jb, jb_entry_t *new_jb_entry);

jb_entry_t *jb_entry_new(uint32_t data_size);
void jb_entry_free(jb_entry_t *jb_entry);

#endif
