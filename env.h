#ifndef MINILISP_ENV_H
#define MINILISP_ENV_H
#include "value.h"

/* Single-linked list of bindings in an environment frame */
typedef struct Binding {
    const char *name;
    Value *value;
    struct Binding *next;
} Binding;

/* Environment = current frame + parent (lexical chain) */
typedef struct Env {
    Binding *head;
    struct Env *parent;
} Env;

/* Constructors */
Env *make_env(Env *parent);
Env *make_global(void);

/* CRUD over environment */
void env_define(Env *env, const char *name,
                Value *value); /* define in current frame */
bool env_set(Env *env, const char *name,
             Value *value);                    /* set in nearest frame */
Value *env_lookup(Env *env, const char *name); /* find in chain or NULL */

#endif /* MINILISP_ENV_H */
