#include <stdlib.h>
#include <string.h>
#include "gaia_utils.h"

int string_to_long(char *string, long *value) {
    char *endptr;
    *value = strtol(string, &endptr, 10);
    if (strlen(endptr) != 0) {
        return -1;
    }
    return 0;
}

int get_port(char *string, port_t *port) {
    long value;
    int err;
    if ((err = string_to_long(string, &value)) < 0) {
        return err;
    }
    *port = value;
    return 0;
}

int get_addr_port(char *arg, in_addr_t *addr, port_t *port) {
    char *token;
    token = strtok(arg, ":");
    if ((*addr = inet_addr(token)) == -1) {
        return -1;
    }
    if ((token = strtok(NULL, ":")) != NULL) {
        int err;
        if ((err = get_port(token, port)) < 0) {
            return err;
        }
    }
    if (strtok(NULL, ":") != NULL) {
        return -1;
    }
    return 0;
}

int set_fifo_scheduling(pthread_attr_t *attr, int8_t priority_offset) {
    int err;

    struct sched_param sched_param;
    if ((err = pthread_attr_getschedparam(attr, &sched_param)) != 0) {
        return err;
    }
    if ((err = pthread_attr_setinheritsched(attr,
                                            PTHREAD_EXPLICIT_SCHED)) != 0) {
        return err;
    }
    if ((err = pthread_attr_setschedpolicy(attr, SCHED_FIFO)) != 0) {
        return err;
    }
    sched_param.sched_priority = sched_get_priority_max(SCHED_FIFO) +
        priority_offset;
    if ((err = pthread_attr_setschedparam(attr, &sched_param)) != 0) {
        return err;
    }
    return 0;
}

int ip4_to_int(int first, int second, int third, int fourth) {
    return (first << 24) | (second << 16) | (third << 8) | (fourth);
}
