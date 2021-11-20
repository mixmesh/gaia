#include <stdio.h>
#include <assert.h>
#include "../jb.h"
#include "../jb_table.h"
#include "../bits.h"

#define UDP_BUF_SIZE 16

/*
 * jb.c test suite
 */

void print_jb(jb_t *jb) {
  jb_entry_t *entry = jb->tail;
  while (entry != NULL) {
    fprintf(stderr, "%d:%s -> ", entry->seqnum, entry->udp_buf);
    entry = entry->next;
  }
  fprintf(stderr, "\n");
  /*
  entry = jb->head;
  while (entry != NULL) {
    fprintf(stderr, "%d:%s <- ", entry->seqnum, entry->udp_buf);
    entry = entry->previous;
  }
  fprintf(stderr, "\n");
  */
}

int main (int argc, char *argv[]) {
  fprintf(stderr, "========================================================================\n");
  fprintf(stderr, "jb_test\n");
  fprintf(stderr, "========================================================================\n");

  jb_t *jb = jb_new(1, false);
  jb_entry_t *jb_entry;
  uint8_t result;

  fprintf(stderr, "* Add 2:2\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 2;
  strcpy((char *)jb_entry->udp_buf, "2");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, FIRST_INSERTED));
  assert(jb->nentries == 1);
  print_jb(jb);

  fprintf(stderr, "* Add 2:2 (again)\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 2;
  strcpy((char *)jb_entry->udp_buf, "2");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, ALREADY_EXISTS));
  assert(jb->nentries == 1);
  jb_entry_free(jb_entry);
  print_jb(jb);

  fprintf(stderr, "* Add 10:10\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 10;
  strcpy((char *)jb_entry->udp_buf, "10");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, TAIL_INSERTED));
  assert(jb->nentries == 2);
  print_jb(jb);

  fprintf(stderr, "* Add 20:20\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 20;
  strcpy((char *)jb_entry->udp_buf, "20");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, TAIL_INSERTED));
  assert(jb->nentries == 3);
  print_jb(jb);

  fprintf(stderr, "* Add 15:15\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 15;
  strcpy((char *)jb_entry->udp_buf, "15");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, INTERMEDIATE_INSERTED));
  assert(jb->nentries == 4);
  print_jb(jb);

  fprintf(stderr, "* Add 5:5\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 5;
  strcpy((char *)jb_entry->udp_buf, "5");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, INTERMEDIATE_INSERTED));
  assert(jb->nentries == 5);
  print_jb(jb);

  fprintf(stderr, "* Add 1:1\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 1;
  strcpy((char *)jb_entry->udp_buf, "1");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, HEAD_INSERTED));
  assert(jb->nentries == 6);
  print_jb(jb);

  fprintf(stderr, "* Add 30:30\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 30;
  strcpy((char *)jb_entry->udp_buf, "30");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, TAIL_INSERTED));
  assert(jb->nentries == 7);
  print_jb(jb);

  fprintf(stderr, "* Add 40:40\n");
  jb_entry = jb_entry_new(UDP_BUF_SIZE, 0);
  jb_entry->seqnum = 40;
  strcpy((char *)jb_entry->udp_buf, "40");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, TAIL_INSERTED));
  assert(jb->nentries == 8);
  print_jb(jb);

  fprintf(stderr, "* Add 50:50\n");
  jb_entry = jb_pop(jb);
  jb_entry->seqnum = 50;
  strcpy((char *)jb_entry->udp_buf, "50");
  result = jb_insert(jb, jb_entry);
  assert(CHK_FLAG(result, TAIL_INSERTED));
  assert(jb->nentries == 8);
  print_jb(jb);

  jb_free(jb, false);

  return 0;
}
