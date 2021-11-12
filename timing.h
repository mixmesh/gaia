#ifndef _TIMING_H
#define _TIMING_H_

#include <inttypes.h>
#include <time.h>

#define SECONDS_BETWEEN_1970_and_2021 1609459200

void timespec_diff(struct timespec *start, struct timespec *stop,
                   struct timespec *diff);
void msleep(uint32_t ms);
uint64_t utimestamp(void);

#endif
