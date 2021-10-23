#include <stdio.h>
#include <stdbool.h>
#include "jb.h"
#include "bits.h"

jb_t *jb_new(char *name) {
  jb_t *jb = malloc(sizeof(jb_t));
  strcpy(jb->name, name);
  jb->entries = 0;
  jb->head = NULL;
  jb->tail = NULL;
  return jb;
}

void jb_free(jb_t *jb) {
  jb_entry_t *jb_entry = jb->head;
  while (jb_entry != NULL) {
    jb_entry_t *next_jb_entry = jb_entry->next;
    jb_entry_free(jb_entry);
    jb_entry = next_jb_entry;
  }
  free(jb);
}

jb_entry_t *jb_pop(jb_t *jb) {
  jb_entry_t *tail = jb->tail;
  if (tail != NULL) {
    if (tail->prev == NULL) {
      jb->entries = 0;
      jb->head = NULL;
      jb->tail = NULL;
    } else {
      --jb->entries;
      tail->prev->next = NULL;
      jb->tail = tail->prev;
    }
    return tail;
  } else {
    return NULL;
  }
}

uint8_t jb_insert(jb_t *jb, jb_entry_t *new_jb_entry) {
  uint8_t flags = 0;
  if (jb->head != NULL) {
    jb_entry_t *jb_entry = jb->head;
    while (true) {
      if (jb_entry != NULL && new_jb_entry->index == jb_entry->index) {
        SET_FLAG(flags, ALREADY_EXISTS);
        break;
      } else if (jb_entry != NULL && new_jb_entry->index < jb_entry->index) {
        jb_entry = jb_entry->next;
      } else {
        if (jb_entry == NULL) {
          // Insert new tail jitter buffer entry
          new_jb_entry->next = NULL;
          new_jb_entry->prev = jb->tail;
          jb->tail->next = new_jb_entry;
          jb->tail = new_jb_entry;
          SET_FLAG(flags, TAIL_INSERTED);
        } else if (jb_entry == jb->head) {
          // Insert new head jitter buffer entry
          jb->head = new_jb_entry;
          new_jb_entry->next = jb_entry;
          new_jb_entry->prev = NULL;
          jb_entry->prev = new_jb_entry;
          SET_FLAG(flags, HEAD_INSERTED);
        } else {
          // Insert new intermediate jitter buffer entry
          new_jb_entry->next = jb_entry;
          new_jb_entry->prev = jb_entry->prev;
          jb_entry->prev->next = new_jb_entry;
          jb_entry->prev = new_jb_entry;
          SET_FLAG(flags, INTERMEDIATE_INSERTED);
        }
        ++jb->entries;
        break;
      }
    }
  } else {
    new_jb_entry->next = NULL;
    new_jb_entry->prev = NULL;
    jb->entries = 1;
    jb->head = new_jb_entry;
    jb->tail = new_jb_entry;
    SET_FLAG(flags, FIRST_INSERTED);
  }
  return flags;
}

jb_entry_t *jb_entry_new(uint32_t data_size) {
  jb_entry_t *jb_entry = malloc(sizeof(jb_entry_t));
  jb_entry->data = malloc(data_size);
  return jb_entry;
}

void jb_entry_free(jb_entry_t *jb_entry) {
  free(jb_entry->data);
  free(jb_entry);
}
