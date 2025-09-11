#include "reader.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Forward declarations */
static Value *read_list(Reader *r);
static Value *read_atom_or_special(Reader *r, int c);
static Value *read_symbol_or_int(Reader *r, int first);
static Value *read_string(Reader *r);

/* Initialize a Reader */
void reader_init(Reader *r, const char *src) {
    r->src = src;
    r->pos = 0;
}

int reader_peek(Reader *r) { 
    return r->src[r->pos]; 
}

int reader_getc(Reader *r) { return r->src[r->pos++]; }

void reader_skip_ws(Reader *r) {
    for (;;) {
        int c = reader_peek(r);
        if (c == ';') {
            while (c && c != '\n') {
                reader_getc(r);
                c = reader_peek(r);
            }
        } else if (isspace(c)) {
            reader_getc(r);
        } else {
            break;
        }
    }
}

/* Read one expression */
Value *read_expr(Reader *r) {
    reader_skip_ws(r);
    int c = reader_peek(r);
    if (c == 0) {
        return NULL;
    }
    if (c == '(') {
        reader_getc(r);
        return read_list(r);
    }
    if (c == '\'') {
        reader_getc(r);
        Value *inner = read_expr(r);
        Value *items[2] = {make_symbol("quote"), inner ? inner : make_nil()};
        return list_from_array(items, 2);
    }
    if (c == '"') {
        reader_getc(r);
        return read_string(r);
    }
    return read_atom_or_special(r, reader_getc(r));
}

/* Parse list: (a b c) */
static Value *read_list(Reader *r) {
    reader_skip_ws(r);
    int c = reader_peek(r);
    if (c == ')') {
        reader_getc(r);
        return make_nil();
    }
    Value *head = NULL;
    Value *tail = NULL;
    while ((c = reader_peek(r))) {
        if (c == ')') {
            reader_getc(r);
            break;
        }
        Value *el = read_expr(r);
        if (!head) {
            head = tail = cons(el, make_nil());
        } else {
            tail->pair.cdr = cons(el, make_nil());
            tail = tail->pair.cdr;
        }
        reader_skip_ws(r);
    }
    if (!head) {
        head = make_nil();
    }
    return head;
}

/* Handle #t / #f or delegate */
static Value *read_atom_or_special(Reader *r, int c) {
    if (c == '#') {
        int d = reader_peek(r);
        if (d == 't' || d == 'T') {
            reader_getc(r);
            return make_bool(true);
        }
        if (d == 'f' || d == 'F') {
            reader_getc(r);
            return make_bool(false);
        }
        fprintf(stderr, "reader: invalid boolean literal after '#'\n");
        return make_nil();
    }
    return read_symbol_or_int(r, c);
}

/* Read numbers and symbols */
static Value *read_symbol_or_int(Reader *r, int first) {
    char buf[256];
    int i = 0;
    buf[i++] = (char)first;
    while (i < (int)sizeof(buf) - 1) {
        int c = reader_peek(r);
        if (c == 0 || isspace(c) || c == '(' || c == ')' || c == '\'' ||
            c == ';' || c == '"') {
            break;
        }
        buf[i++] = (char)reader_getc(r);
    }
    buf[i] = 0;

    int j = 0;
    if (buf[0] == '-') {
        j = 1;
    }
    int isnum = (buf[j] != 0);
    for (; buf[j]; ++j) {
        if (!isdigit((unsigned char)buf[j])) {
            isnum = 0;
            break;
        }
    }

    if (isnum) {
        long v = strtol(buf, NULL, 10);
        return make_int(v);
    }

    if (strcmp(buf, "nil") == 0) {
        return make_nil();
    }
    return make_symbol(buf);
}

/* Read string literal: assumes initial '"' already consumed */
static Value *read_string(Reader *r) {
    char buf[1024];
    size_t i = 0;
    for (;;) {
        int c = reader_getc(r);
        if (c == 0) {
            fprintf(stderr, "reader: unterminated string\n");
            break;
        }
        if (c == '"') {
            break;
        }
        if (c == '\\') {
            int d = reader_getc(r);
            if (d == 'n') {
                buf[i++] = '\n';
            } else if (d == 'r') {
                buf[i++] = '\r';
            } else if (d == 't') {
                buf[i++] = '\t';
            } else if (d == '"' || d == '\\') {
                buf[i++] = (char)d;
            } else {
                /* Unknown escape: keep literally */
                buf[i++] = (char)d;
            }
        } else {
            buf[i++] = (char)c;
        }
        if (i >= sizeof(buf) - 1) {
            fprintf(stderr, "reader: string too long\n");
            break;
        }
    }
    buf[i] = 0;
    return make_string_copy(buf, i);
}
