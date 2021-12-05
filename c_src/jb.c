#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include "jb.h"
#include "bits.h"
#include "globals.h"
#include "gaia_utils.h"

jb_t *jb_new(uint32_t gaia_id, bool opus_enabled) {
    jb_t *jb = malloc(sizeof(jb_t));
    jb->gaia_id = gaia_id;
    jb->playback = NULL;
    jb->playback_seqnum = 0;
    jb->exhausted = false;
    jb->nentries = 0;
    jb->rwlock = malloc(sizeof(thread_rwlock_t));
    assert(thread_rwlock_init(jb->rwlock, "jb") == 0);
    jb->npeak_values = PEAK_AVERAGE_PERIOD_IN_MS / PERIOD_SIZE_IN_MS;
    jb->peak_values = calloc(jb->npeak_values, sizeof(uint16_t));
    jb->peak_index = 0;
    jb->peak_average = 0;
    if (opus_enabled) {
        int err;
        jb->opus_decoder = opus_decoder_create(RATE_IN_HZ, CHANNELS, &err);
        if (err < 0) {
            DEBUGF("ERROR: Failed to create decoder: %s\n", opus_strerror(err));
        }
        assert(err == 0);
    } else {
        jb->opus_decoder = NULL;
    }
    jb->tail = NULL;
    jb->head = NULL;
    return jb;
    }

void jb_free(jb_t *jb, bool free_entries_only) {
    if (jb->nentries == 0 || jb->exhausted) {
        return;
    }
    jb_take_wrlock(jb);
    jb_entry_t *jb_entry = jb->tail;
    while (jb_entry != NULL) {
        jb_entry_t *next_jb_entry = jb_entry->next;
        jb_entry_free(jb_entry);
        jb_entry = next_jb_entry;
    }
    if (jb->opus_decoder != NULL) {
        opus_decoder_destroy(jb->opus_decoder);
    }
    jb->tail = NULL;
    jb->head = NULL;
    if (free_entries_only) {
        jb->playback = NULL;
        jb->playback_seqnum = 0;
        jb->exhausted = false;
        jb->nentries = 0;
        jb_release_wrlock(jb);
    } else {
        jb_release_wrlock(jb);
        assert(thread_rwlock_destroy(jb->rwlock) == 0);
        free(jb->rwlock);
        free(jb->peak_values);
        free(jb);
    }
}

jb_entry_t *jb_pop(jb_t *jb) {
    jb_entry_t *head = jb->head;
    if (head != NULL) {
        if (head->prev == NULL) {
            jb->nentries = 0;
            jb->tail = NULL;
            jb->head = NULL;
        } else {
            --jb->nentries;
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
                ++jb->nentries;
                break;
            }
        }
    } else {
        new_jb_entry->next = NULL;
        new_jb_entry->prev = NULL;
        jb->nentries = 1;
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

uint32_t jb_get_index(jb_t *jb, jb_entry_t *jb_entry) {
    uint32_t index = 0;
    jb_entry_t *current_jb_entry = jb->tail;
    while (current_jb_entry != jb_entry && current_jb_entry != NULL) {
        index++;
        current_jb_entry = current_jb_entry->next;
    }
    return index;
}

void jb_take_rdlock(jb_t *jb) {
    assert(thread_rwlock_rdlock(jb->rwlock) == 0);
}

void jb_take_wrlock(jb_t *jb) {
    assert(thread_rwlock_wrlock(jb->rwlock) == 0);
}

void jb_release_rdlock(jb_t *jb) {
    assert(thread_rwlock_rdunlock(jb->rwlock) == 0);
}

void jb_release_wrlock(jb_t *jb) {
    assert(thread_rwlock_wrunlock(jb->rwlock) == 0);
}

jb_entry_t *jb_entry_new(uint16_t udp_buf_size, uint16_t period_buf_size) {
    jb_entry_t *jb_entry = malloc(sizeof(jb_entry_t));
    jb_entry->seqnum = 0;
    jb_entry->udp_buf = malloc(udp_buf_size);
    jb_entry->period_buf = malloc(period_buf_size);
    return jb_entry;
}

void jb_entry_free(jb_entry_t *jb_entry) {
    free(jb_entry->udp_buf);
    free(jb_entry->period_buf);
    free(jb_entry);
}
