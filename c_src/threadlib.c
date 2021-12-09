#include "threadlib.h"
#include "globals.h"

int thread_rwlock_init(thread_rwlock_t *rwlock, char *name) {
#ifdef NIF
    rwlock->enif_rwlock = enif_rwlock_create(name);
    return rwlock->enif_rwlock == NULL;
#else
    rwlock->pthread_rwlock = malloc(sizeof(pthread_rwlock_t));
    return pthread_rwlock_init(rwlock->pthread_rwlock, NULL);
#endif
}

int thread_rwlock_destroy(thread_rwlock_t *rwlock) {
#ifdef NIF
    enif_rwlock_destroy(rwlock->enif_rwlock);
    return 0;
#else
    int res = pthread_rwlock_destroy(rwlock->pthread_rwlock);
    free(rwlock->pthread_rwlock);
    return res;
#endif
}

int thread_rwlock_rdlock(thread_rwlock_t *rwlock) {
#ifdef NIF
    enif_rwlock_rlock(rwlock->enif_rwlock);
    return 0;
#else
    return pthread_rwlock_rdlock(rwlock->pthread_rwlock);
#endif
}

int thread_rwlock_wrlock(thread_rwlock_t *rwlock) {
#ifdef NIF
    enif_rwlock_rwlock(rwlock->enif_rwlock);
    return 0;
#else
    return pthread_rwlock_wrlock(rwlock->pthread_rwlock);
#endif
}

int thread_rwlock_rdunlock(thread_rwlock_t *rwlock) {
#ifdef NIF
    enif_rwlock_runlock(rwlock->enif_rwlock);
    return 0;
#else
    return pthread_rwlock_unlock(rwlock->pthread_rwlock);
#endif
}

int thread_rwlock_wrunlock(thread_rwlock_t *rwlock) {
#ifdef NIF
    enif_rwlock_rwunlock(rwlock->enif_rwlock);
    return 0;
#else
    return pthread_rwlock_unlock(rwlock->pthread_rwlock);
#endif
}

int thread_mutex_init(thread_mutex_t *mutex, char *name) {
#ifdef NIF
    mutex->enif_mutex = enif_mutex_create(name);
    return mutex->enif_mutex == NULL;
#else
    mutex->pthread_mutex = malloc(sizeof(pthread_mutex_t));
    return pthread_mutex_init(mutex->pthread_mutex, NULL);
#endif
}

int thread_mutex_destroy(thread_mutex_t *mutex) {
#ifdef NIF
    enif_mutex_destroy(mutex->enif_mutex);
    return 0;
#else
    int res = pthread_mutex_destroy(mutex->pthread_mutex);
    free(mutex->pthread_mutex);
    return res;
#endif
}

int thread_mutex_lock(thread_mutex_t *mutex) {
#ifdef NIF
    enif_mutex_lock(mutex->enif_mutex);
    return 0;
#else
    return pthread_mutex_lock(mutex->pthread_mutex);
#endif
}

int thread_mutex_unlock(thread_mutex_t *mutex) {
#ifdef NIF
    enif_mutex_unlock(mutex->enif_mutex);
    return 0;
#else
    return pthread_mutex_unlock(mutex->pthread_mutex);
#endif
}

void thread_exit(void *retval) {
#ifdef NIF
    enif_thread_exit(retval);
#else
    exit(INTERNAL_ERROR);
    //pthread_exit(retval);
#endif
}
