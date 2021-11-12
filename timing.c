#include <assert.h>
#include "timing.h"

void timespec_diff(struct timespec *start, struct timespec *stop,
                   struct timespec *diff) {
    if (stop->tv_nsec - start->tv_nsec < 0) {
        diff->tv_sec = stop->tv_sec - start->tv_sec - 1;
        diff->tv_nsec = stop->tv_nsec - start->tv_nsec + 1000000000;
    } else {
        diff->tv_sec = stop->tv_sec - start->tv_sec;
        diff->tv_nsec = stop->tv_nsec - start->tv_nsec;
    }
}

void msleep(uint32_t ms) {
    struct timespec req = 
        {
         .tv_sec = ms / 1000,
         .tv_nsec = (ms % 1000) * 1000000L
        };
    nanosleep(&req, NULL);
}

uint64_t utimestamp() {
  struct timespec now;
  assert(clock_gettime(CLOCK_REALTIME, &now) == 0);
  return (uint64_t)(now.tv_sec - SECONDS_BETWEEN_1970_and_2021) * 1000000 +
    now.tv_nsec / 1000;
}
