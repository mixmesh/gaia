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
  struct timespec now;
  assert(clock_gettime(CLOCK_REALTIME, &now) == 0);
  return (now.tv_sec - SECONDS_BETWEEN_1970_and_2021) * 1000000 + now.tv_nsec / (double)1000;
}
