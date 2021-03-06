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

uint64_t utimestamp() {
    struct timespec now;
    assert(clock_gettime(CLOCK_REALTIME, &now) == 0);
    return (uint64_t)(now.tv_sec - SECONDS_BETWEEN_1970_and_2021) * 1000000L +
        now.tv_nsec / 1000L;
}
