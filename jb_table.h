#ifndef _JB_TABLE_H_
#define _JB_TABLE_H_

#include "jb.h"

#define JB_TABLE_SUCCESS 0
#define JB_TABLE_ALREADY_EXISTS -1

jb_t *jb_table_new(void);
void jb_table_free(jb_t *jb_table);
int8_t jb_table_add(jb_t **jb_table, jb_t *jb);
jb_t *jb_table_find(jb_t **jb_table, uint32_t userid);
void jb_table_delete(jb_t **jb_table, uint32_t userid);
uint16_t jb_table_count(jb_t **jb_table);
void jb_table_foreach(jb_t *jb_table, void (*callback)(jb_t *t));
  
#endif
