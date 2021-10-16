#ifndef _JB_TABLE_H_
#define _JB_TABLE_H_

#include "jb.h"

jb_t *jb_table_new(void);
void jb_table_add(jb_t **jb_table, jb_t *jb);
jb_t *jb_table_find(jb_t **jb_table, char *name);
void jb_table_delete(jb_t **jb_table, char *name);
uint16_t jb_table_count(jb_t **jb_table);

#endif
