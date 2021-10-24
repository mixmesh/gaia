#ifndef _TIMING_H
#define _TIMING_H_

#include <inttypes.h>

#define SECONDS_BETWEEN_1970_and_2021 1609459200

void msleep(uint32_t ms);
uint64_t utimestamp(void);

#endif
