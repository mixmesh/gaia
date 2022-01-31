#include <unistd.h>
    #include "conversation.h"

conversation_t *conversation_new(void) {
    conversation_t *conversation = malloc(sizeof(conversation_t));
    return conversation;
}

void conversation_free(conversation_t *conversation) {
    close(conversation->sockfd);
    free(conversation);
}
