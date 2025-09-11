#ifndef MINILISP_EVAL_H
#define MINILISP_EVAL_H
#include "env.h"
#include "value.h"

/* Evaluate a single expression in the given environment */
Value *eval(Value *expr, Env *env);

/* Evaluate each element of a list (left-to-right), returning a new list */
Value *eval_list(Value *list, Env *env);

/* Apply a function (builtin or closure) to an already-evaluated arg list */
Value *apply(Value *fn, Value *args, Env *env);

#endif /* MINILISP_EVAL_H */
