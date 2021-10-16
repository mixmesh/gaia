#include <stdio.h>
#include <assert.h>
#include "../jb_table.h"

int main (int argc, char *argv[]) {
  fprintf(stderr, "========================================================================\n");
  fprintf(stderr, "jb_table_test\n");
  fprintf(stderr, "========================================================================\n");

  jb_t *jb_table = jb_table_new();
  jb_t *jb;

  jb = jb_new("foo");
  jb_table_add(&jb_table, jb);
  assert(jb_table_count(&jb_table) == 1);

  jb = jb_new("bar");
  jb_table_add(&jb_table, jb);
  assert(jb_table_count(&jb_table) == 2);

  jb = jb_table_find(&jb_table, "foo");
  assert(jb != NULL);

  jb = jb_table_find(&jb_table, "baz");
  assert(jb == NULL);

  for (jb = jb_table; jb != NULL; jb = jb->hh.next) {
    printf("Jitter buffer %s (%d)\n", jb->name, jb->entries);
  }
  
  jb_table_delete(&jb_table, "foo");
  assert(jb_table_count(&jb_table) == 1);
  jb = jb_table_find(&jb_table, "foo");
  assert(jb == NULL);

  return 0;
}
