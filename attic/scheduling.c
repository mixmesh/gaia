#include <pthreads.h>
#include "scheduling.h"

int set_fifo_scheduling(bool threaded) {
  if (threaded) {
    struct sched_param sched_param;
    int err;
    if ((err = pthread_getschedparam(0, &sched_param)) < 0) {
      return err;
    }
    sched_param.sched_priority = sched_get_priority_max(SCHED_FIFO);
    if ((err = pthread_setschedparam(0, SCHED_FIFO, &sched_param)) < 0) {
      return err;
    }
    return sched_param.sched_priority;
  } else {
    struct sched_param sched_param;
    int err;
    if ((err = sched_getparam(0, &sched_param)) < 0) {
      return err;
    }
    sched_param.sched_priority = sched_get_priority_max(SCHED_FIFO);
    if ((err = sched_setscheduler(0, SCHED_FIFO, &sched_param)) < 0) {
      return err;
    }
    return sched_param.sched_priority;
  }
}
