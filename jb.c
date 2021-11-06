#include <stdbool.h>
#include <assert.h>
#include "jb.h"
#include "bits.h"

jb_t *jb_new(uint32_t userid) {
  jb_t *jb = malloc(sizeof(jb_t));
  jb->userid = userid;
  jb->playback = NULL;
  jb->seqnum = 0;
  jb->entries = 0;
  jb->rwlock = malloc(sizeof(pthread_rwlock_t));
  assert(pthread_rwlock_init(jb->rwlock, NULL) == 0);
  jb->tail = NULL;
  jb->head = NULL;
  return jb;
}

void jb_free(jb_t *jb) {
  jb_entry_t *jb_entry = jb->tail;
  while (jb_entry != NULL) {
    jb_entry_t *next_jb_entry = jb_entry->next;
    jb_entry_free(jb_entry);
    jb_entry = next_jb_entry;
  }
  assert(pthread_rwlock_destroy(jb->rwlock) == 0);
  free(jb->rwlock);
  free(jb);
}

jb_entry_t *jb_pop(jb_t *jb) {
  jb_entry_t *head = jb->head;
  if (head != NULL) {
    if (head->prev == NULL) {
      jb->entries = 0;
      jb->tail = NULL;
      jb->head = NULL;
    } else {
      --jb->entries;
      head->prev->next = NULL;
      jb->head = head->prev;
    }
    return head;
  } else {
    return NULL;
  }
}

uint8_t jb_insert(jb_t *jb, jb_entry_t *new_jb_entry) {
  uint8_t flags = 0;
  if (jb->tail != NULL) {
    jb_entry_t *jb_entry = jb->tail;
    while (true) {
      if (jb_entry != NULL &&
          new_jb_entry->seqnum == jb_entry->seqnum) {
        SET_FLAG(flags, ALREADY_EXISTS);
        break;
      } else if (jb_entry != NULL &&
                 new_jb_entry->seqnum < jb_entry->seqnum) {
        jb_entry = jb_entry->next;
      } else {
        if (jb_entry == NULL) {
          // Insert new head jitter buffer entry
          new_jb_entry->next = NULL;
          new_jb_entry->prev = jb->head;
          jb->head->next = new_jb_entry;
          jb->head = new_jb_entry;
          SET_FLAG(flags, HEAD_INSERTED);
        } else if (jb_entry == jb->tail) {
          // Insert new tail jitter buffer entry
          jb->tail = new_jb_entry;
          new_jb_entry->next = jb_entry;
          new_jb_entry->prev = NULL;
          jb_entry->prev = new_jb_entry;
          SET_FLAG(flags, TAIL_INSERTED);
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
    jb->tail = new_jb_entry;
    jb->head = new_jb_entry;
    SET_FLAG(flags, FIRST_INSERTED);
  }
  return flags;
}

jb_entry_t *jb_get_entry(jb_t *jb, uint32_t index) {
  jb_entry_t *jb_entry = jb->tail;
  while (jb_entry != NULL) {
    if (index-- == 0) {
      return jb_entry;
    }
    jb_entry = jb_entry->next;
  }
  return NULL;
}

void jb_take_rdlock(jb_t *jb) {
  assert(pthread_rwlock_rdlock(jb->rwlock) == 0);
}

void jb_take_wrlock(jb_t *jb) {
  assert(pthread_rwlock_wrlock(jb->rwlock) == 0);
}

void jb_release_lock(jb_t *jb) {
  assert(pthread_rwlock_unlock(jb->rwlock) == 0);
}

jb_entry_t *jb_entry_new(uint32_t data_size) {
  jb_entry_t *jb_entry = malloc(sizeof(jb_entry_t));
  jb_entry->seqnum = 0;
  jb_entry->data = malloc(data_size);
  return jb_entry;
}

void jb_entry_free(jb_entry_t *jb_entry) {
  free(jb_entry->data);
  free(jb_entry);
}
