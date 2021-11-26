#ifndef _TIMING_H
#define _TIMING_H_

#include <inttypes.h>
#include <time.h>

// Borrowed from sys/time.h (only available on BSD)
#define	timespecclear(tsp) (tsp)->tv_sec = (tsp)->tv_nsec = 0
#define	timespecisset(tsp) ((tsp)->tv_sec || (tsp)->tv_nsec)
#define	timespecisvalid(tsp) \
    ((tsp)->tv_nsec >= 0 && (tsp)->tv_nsec < 1000000000L)
#define	timespeccmp(tsp, usp, cmp) \
    (((tsp)->tv_sec == (usp)->tv_sec) ? \
     ((tsp)->tv_nsec cmp (usp)->tv_nsec) : \
     ((tsp)->tv_sec cmp (usp)->tv_sec))
#define	timespecadd(tsp, usp, vsp) \
    do { \
        (vsp)->tv_sec = (tsp)->tv_sec + (usp)->tv_sec; \
        (vsp)->tv_nsec = (tsp)->tv_nsec + (usp)->tv_nsec; \
        if ((vsp)->tv_nsec >= 1000000000L) { \
            (vsp)->tv_sec++; \
            (vsp)->tv_nsec -= 1000000000L; \
        } \
    } while (0)
#define	timespecsub(tsp, usp, vsp) \
    do { \
        (vsp)->tv_sec = (tsp)->tv_sec - (usp)->tv_sec; \
        (vsp)->tv_nsec = (tsp)->tv_nsec - (usp)->tv_nsec; \
        if ((vsp)->tv_nsec < 0) { \
            (vsp)->tv_sec--; \
            (vsp)->tv_nsec += 1000000000L; \
        } \
    } while (0)

#define SECONDS_BETWEEN_1970_and_2021 1609459200

void msleep(uint32_t ms);
uint64_t utimestamp(void);

#endif
