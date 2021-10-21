#include <time.h>
#include <assert.h>
#include "timing.h"

void msleep(uint32_t ms) {
  struct timespec req = 
    {
     .tv_sec = ms / 1000,
     .tv_nsec = (ms % 1000) * 1000000L
    };
  nanosleep(&req, NULL);
}

uint32_t get_timestamp() {
  struct timespec tp;
  assert(clock_gettime(CLOCK_REALTIME, &tp) == 0);
  return tp.tv_sec * 1000 + tp.tv_nsec * 1000;
}
