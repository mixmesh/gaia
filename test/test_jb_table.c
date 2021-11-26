#include <stdio.h>
#include <assert.h>
#include "../jb_table.h"

int main (int argc, char *argv[]) {
  fprintf(stderr, "========================================================================\n");
  fprintf(stderr, "jb_table_test\n");
  fprintf(stderr, "========================================================================\n");

  jb_table_t *jb_table = jb_table_new();
  jb_t *jb;
  int8_t res;

  jb = jb_new(1, false);
  res = jb_table_add(jb_table, jb);
  assert(res == JB_TABLE_SUCCESS && jb_table_count(jb_table) == 1);

  jb = jb_new(2, false);
  res = jb_table_add(jb_table, jb);
  assert(res == JB_TABLE_SUCCESS && jb_table_count(jb_table) == 2);

  jb = jb_table_find(jb_table, 1);
  assert(jb != NULL);

  jb = jb_table_find(jb_table, 3);
  assert(jb == NULL);

  for (jb = jb_table->jb; jb != NULL; jb = jb->hh.next) {
    printf("Jitter buffer %d (%d)\n", jb->userid, jb->nentries);
  }

  jb_table_delete(jb_table, 1);
  assert(jb_table_count(jb_table) == 1);
  jb = jb_table_find(jb_table, 1);
  assert(jb == NULL);

  return 0;
}
