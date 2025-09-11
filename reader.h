#ifndef MINILISP_READER_H
#define MINILISP_READER_H
#include "value.h"
#include <stddef.h>

/* Reader = cursor over a source string */
typedef struct {
    const char *src;
    size_t pos;
} Reader;


void reader_init(Reader *r, const char *src);

/* Leer caracteres */
int reader_peek(Reader *r);     /* peek at the next chat (0 = fin) */
int reader_getc(Reader *r);     /* consume next char */
void reader_skip_ws(Reader *r); /* jump spaces and comments ;... */

/* read a lips expression */
Value *read_expr(Reader *r);

#endif /* MINILISP_READER_H */
