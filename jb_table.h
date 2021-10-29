#ifndef _JB_TABLE_H_
#define _JB_TABLE_H_

#include "jb.h"
#include "uthash/uthash.h"

#define JB_TABLE_SUCCESS 0
#define JB_TABLE_ALREADY_EXISTS -1

#define HASH_FIND_UINT32(head, findint, out) \
HASH_FIND(hh, head, findint, sizeof(uint32_t), out)
#define HASH_ADD_UINT32(head, intfield, add) \
HASH_ADD(hh, head, intfield, sizeof(uint32_t), add)

jb_t *jb_table_new(void);
int8_t jb_table_add(jb_t **jb_table, jb_t *jb);
jb_t *jb_table_find(jb_t **jb_table, uint32_t userid);
void jb_table_delete(jb_t **jb_table, uint32_t userid);
uint16_t jb_table_count(jb_t **jb_table);

#endif
