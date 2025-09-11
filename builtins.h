#ifndef MINILISP_BUILTINS_H
#define MINILISP_BUILTINS_H
#include "env.h"
#include "value.h"

/* Install all builtin functions into the given environment */
void install_builtins(Env *env);

#endif /* MINILISP_BUILTINS_H */
