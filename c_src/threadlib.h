#ifndef _THREADLIB_H_
#define _THREADLIB_H_

#include <erl_nif.h>
#include <pthread.h>

typedef union {
    ErlNifRWLock *enif_rwlock;
    pthread_rwlock_t *pthread_rwlock;
} thread_rwlock_t;

typedef union {
    ErlNifMutex *enif_mutex;
    pthread_mutex_t *pthread_mutex;
} thread_mutex_t;

typedef union {
    ErlNifCond *enif_cond;
    pthread_cond_t *pthread_cond;
} thread_cond_t;

int thread_rwlock_init(thread_rwlock_t *rwlock, char *name);
int thread_rwlock_destroy(thread_rwlock_t *rwlock);
int thread_rwlock_rdlock(thread_rwlock_t *rwlock);
int thread_rwlock_wrlock(thread_rwlock_t *rwlock);
int thread_rwlock_rdunlock(thread_rwlock_t *rwlock);
int thread_rwlock_wrunlock(thread_rwlock_t *rwlock);
int thread_mutex_init(thread_mutex_t *mutex, char *name);
int thread_mutex_destroy(thread_mutex_t *mutex);
int thread_mutex_lock(thread_mutex_t *mutex);
int thread_mutex_unlock(thread_mutex_t *mutex);
int thread_cond_init(thread_cond_t *cond, char *name);
int thread_cond_destroy(thread_cond_t *cond);
int thread_cond_wait(thread_cond_t *cond, thread_mutex_t *mutex);
int thread_cond_signal(thread_cond_t *cond);
void thread_exit(void *retval);

#endif
