#include "env.h"
#include "builtins.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Portable strdup replacement (avoids missing prototype under strict C standards) */
static char* xstrdup(const char* s) {
    if (!s) return NULL;
    size_t n = strlen(s) + 1;
    char* p = (char*)malloc(n);
    if (!p) { fprintf(stderr, "OOM\n"); exit(1); }
    memcpy(p, s, n);
    return p;
}


/* Create a new environment frame with an optional parent */
Env *make_env(Env *parent) {
    Env *e = (Env *)malloc(sizeof(Env));
    if (!e) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    e->head = NULL;
    e->parent = parent;
    return e;
}

/* Define a new binding in the current frame */
void env_define(Env *env, const char *name, Value *value) {
    Binding *b = (Binding *)malloc(sizeof(Binding));
    if (!b) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    b->name = xstrdup(name);
    if (!b->name) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    b->value = value;
    b->next = env->head;
    env->head = b;
}

/* Set an existing binding in the nearest frame in the chain */
bool env_set(Env *env, const char *name, Value *value) {
    for (Env *e = env; e; e = e->parent) {
        for (Binding *b = e->head; b; b = b->next) {
            if (strcmp(b->name, name) == 0) {
                b->value = value;
                return true;
            }
        }
    }
    return false;
}

/* Lookup a name across the chain */
Value *env_lookup(Env *env, const char *name) {
    for (Env *e = env; e; e = e->parent) {
        for (Binding *b = e->head; b; b = b->next) {
            if (strcmp(b->name, name) == 0)
                return b->value;
        }
    }
    return NULL;
}

/* Global environment with core constants and builtins installed */
Env *make_global(void) {
    Env *g = make_env(NULL);
    env_define(g, "#t", make_bool(true));
    env_define(g, "#f", make_bool(false));
    env_define(g, "nil", make_nil());
    install_builtins(g); /* from builtins.c */
    return g;
}
