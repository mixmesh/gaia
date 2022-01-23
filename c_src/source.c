#include <unistd.h>
    #include "source.h"

source_t *source_new(void) {
    source_t *source = malloc(sizeof(source_t));
    return source;
}

void source_free(source_t *source) {
    close(source->sockfd);
    free(source);
}
