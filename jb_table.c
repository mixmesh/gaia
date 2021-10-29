#include "jb_table.h"

jb_t *jb_table_new(void) {
  return NULL;
}

int8_t jb_table_add(jb_t **jb_table, jb_t *jb) {
  jb_t *existing_jb;
  HASH_FIND_INT(*jb_table, &(jb->userid), existing_jb);
  if (existing_jb != NULL) {
    return JB_TABLE_ALREADY_EXISTS;
  }
  HASH_ADD_INT(*jb_table, userid, jb);
  return JB_TABLE_SUCCESS;
}

jb_t *jb_table_find(jb_t **jb_table, int32_t userid) {
  jb_t *jb;
  HASH_FIND_INT(*jb_table, &userid, jb);
  return jb;
}

void jb_table_delete(jb_t **jb_table, int32_t userid) {
  jb_t *jb;
  HASH_FIND_INT(*jb_table, &userid, jb);
  if (jb != NULL) {
    HASH_DEL(*jb_table, jb);
    jb_free(jb);
  }
}
 
uint16_t jb_table_count(jb_t **jb_table) {
  return HASH_COUNT(*jb_table);
}
