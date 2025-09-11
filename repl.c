#include "repl.h"
#include "reader.h"
#include "eval.h"
#include "value.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>
#define LINE_MAX_LEN 2048

/* REPL prints results; file loader keeps this off to avoid <closure> spam. */
static bool echo_results = true;

/* Simple paren balancer used by the REPL's multi-line input. */
static int paren_balance(const char* s) {
    int bal = 0;
    for (const char* p = s; *p; ++p) {
        if (*p == '(') { ++bal; }
        else if (*p == ')') { --bal; }
    }
    return bal;
}

/* -------- File execution (no echo) -------- */


/* Execute all forms in the given file path.
   Returns 0 on success, non-zero on open/read error. */
int run_file(Env* env, const char* path) {
    bool from_stdin = (path && strcmp(path, "-") == 0);

    /* Open file (or use stdin). Print precise diagnostics on failure. */
    FILE* f = NULL;
    if (from_stdin) {
        f = stdin;
    } else {
        if (!path || path[0] == '\0') {
            fprintf(stderr, "run_file: empty path\n");
            return 1;
        }
        f = fopen(path, "rb");
        if (!f) {
            fprintf(stderr, "fopen('%s'): %s\n", path, strerror(errno));
            return 1;
        }
    }

    /* Read whole input into a buffer because our Reader API takes a C string */
    char* buf = NULL;
    size_t cap = 0, len = 0;
    const size_t CHUNK = 64 * 1024;

    for (;;) {
        if (len + CHUNK + 1 > cap) {
            size_t newcap = (cap == 0 ? CHUNK : cap * 2);
            char* nb = (char*)realloc(buf, newcap);
            if (!nb) {
                if (!from_stdin) { fclose(f); }
                free(buf);
                fprintf(stderr, "run_file('%s'): out of memory\n", from_stdin ? "<stdin>" : path);
                return 1;
            }
            buf = nb;
            cap = newcap;
        }
        size_t n = fread(buf + len, 1, CHUNK, f);
        len += n;
        if (n < CHUNK) {
            if (feof(f)) { break; }
            if (ferror(f)) {
                fprintf(stderr, "read('%s'): %s\n", from_stdin ? "<stdin>" : path, strerror(errno));
                if (!from_stdin) { fclose(f); }
                free(buf);
                return 1;
            }
        }
    }
    if (!from_stdin) { fclose(f); }
    if (buf == NULL) { /* empty file */
        buf = (char*)malloc(1);
        if (!buf) { fprintf(stderr, "OOM\n"); return 1; }
        buf[0] = '\0';
    } else {
        buf[len] = '\0';
    }

    /* Execute without echo to avoid <closure> spam */
    bool prev = echo_results;
    echo_results = false;

    Reader r;
    reader_init(&r, buf);
    for (;;) {
        Value* expr = read_expr(&r);
        if (!expr) { break; }
        Value* result = eval(expr, env);
        if (echo_results) {
            print_value(result);
            printf("\n");
        }
        (void)result;
    }

    echo_results = prev;
    free(buf);
    return 0;
}

/* -------- Interactive REPL (with echo) -------- */
void run_repl(Env* global) {
    echo_results = true;
    printf("Mini-Lisp REPL. Ctrl-D to exit.\n");

    for (;;) {
        char buf[LINE_MAX_LEN];
        buf[0] = '\0';

        printf("> ");
        fflush(stdout);
        if (!fgets(buf, sizeof(buf), stdin)) {
            printf("\n");
            break; /* EOF */
        }

        /* If parentheses are unbalanced, keep reading continuation lines. */
        while (paren_balance(buf) > 0) {
            char cont[LINE_MAX_LEN];
            printf(".. ");
            fflush(stdout);
            if (!fgets(cont, sizeof(cont), stdin)) {
                break;
            }
            strncat(buf, cont, sizeof(buf) - strlen(buf) - 1);
        }

        Reader r;
        reader_init(&r, buf);
        Value* expr = read_expr(&r);
        if (!expr) {
            continue;
        }

        Value* out = eval(expr, global);
        if (echo_results) {
            print_value(out);
            printf("\n");
        }
    }

    printf("\nBye.\n");
}
