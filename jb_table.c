#include "jb.h"

jb_t *jb_table_new(void) {
  return NULL;
}

void jb_table_add(jb_t **jb_table, jb_t *jb) {
  HASH_ADD_STR(*jb_table, name, jb);
}

jb_t *jb_table_find(jb_t **jb_table, char *name) {
  jb_t *jb;
  HASH_FIND_STR(*jb_table, name, jb);
  return jb;
}

void jb_table_delete(jb_t **jb_table, char *name) {
  jb_t *jb;
  HASH_FIND_STR(*jb_table, name, jb);
  if (jb != NULL) {
    HASH_DEL(*jb_table, jb);
    jb_free(jb);
  }
}
 
uint16_t jb_table_count(jb_t **jb_table) {
  return HASH_COUNT(*jb_table);
}
