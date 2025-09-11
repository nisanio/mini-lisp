#include "eval.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "macros.h"
#include "syntax.h"

/* Error helpers */

static Value* make_error(const char* msg) {
    fprintf(stderr, "error: %s\n", msg);
    return make_nil();
}

static int count_args(Value* list) {
    return list_length(list);
}

static int ensure_arity_exact(const char* who, Value* args, int expected) {
    int n = count_args(args);
    if (n != expected) {
        char buf[128];
        snprintf(buf, sizeof(buf), "%s: expected %d argument(s), got %d", who, expected, n);
        make_error(buf);
        return 0;
    }
    return 1;
}

__attribute__((unused))
static int ensure_arity_range(const char* who, Value* args, int minv, int maxv) {
    int n = count_args(args);
    if (n < minv || n > maxv) {
        char buf[128];
        snprintf(buf, sizeof(buf), "%s: expected %d..%d arguments, got %d", who, minv, maxv, n);
        make_error(buf);
        return 0;
    }
    return 1;
}

static int ensure_symbol(const char* who, Value* v) {
    if (!v || v->type != VAL_SYMBOL) {
        char buf[128];
        snprintf(buf, sizeof(buf), "%s: expected symbol", who);
        make_error(buf);
        return 0;
    }
    return 1;
}

static int ensure_param_list_symbols(const char* who, Value* params) {
    for (Value* it = params; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        Value* p = it->pair.car;
        if (!p || p->type != VAL_SYMBOL) {
            char buf[128];
            snprintf(buf, sizeof(buf), "%s: parameter list must contain symbols", who);
            make_error(buf);
            return 0;
        }
    }
    if (params && params->type != VAL_NIL && params->type != VAL_PAIR) {
        char buf[128];
        snprintf(buf, sizeof(buf), "%s: parameter list must be a proper list", who);
        make_error(buf);
        return 0;
    }
    return 1;
}

/* Eval helpers */

static Value* eval_sequence(Value* forms, Env* env) {
    Value* result = make_nil();
    for (Value* it = forms; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        result = eval(it->pair.car, env);
    }
    return result;
}

/* Binding list validation helpers for let / let* */

static int is_binding_pair(Value* b) {
    if (!b || b->type != VAL_PAIR) {
        return 0;
    }
    Value* name = car(b);
    Value* rest = cdr(b);
    if (!name || name->type != VAL_SYMBOL) {
        return 0;
    }
    if (!rest || rest->type != VAL_PAIR) {
        return 0;
    }
    if (cdr(rest) && cdr(rest)->type != VAL_NIL) {
        return 0;
    }
    return 1;
}

static int validate_binding_list(Value* bindings, const char* who) {
    for (Value* it = bindings; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        if (!is_binding_pair(it->pair.car)) {
            char buf[128];
            snprintf(buf, sizeof(buf), "%s: each binding must be (name expr)", who);
            make_error(buf);
            return 0;
        }
    }
    if (bindings && bindings->type != VAL_NIL && bindings->type != VAL_PAIR) {
        char buf[128];
        snprintf(buf, sizeof(buf), "%s: binding list must be a proper list", who);
        make_error(buf);
        return 0;
    }
    return 1;
}

/* Core eval */

Value* eval(Value* expr, Env* env) {
    if (!expr) {
        return make_nil();
    }

    switch (expr->type) {
        case VAL_NIL:
        case VAL_INT:
        case VAL_BOOL:
        case VAL_BUILTIN:
        case VAL_CLOSURE:
        case VAL_STRING:
        case VAL_VECTOR:
        case VAL_HASH: {
            return expr;
        }
        case VAL_SYMBOL: {
            Value* v = env_lookup(env, expr->sym);
            if (!v) {
                fprintf(stderr, "unbound symbol: %s\n", expr->sym);
                return make_nil();
            }
            return v;
        }
        case VAL_PAIR: {
            /* === Macro expansion BEFORE analyzing the pair ===
               1) Hygienic attempt.
               2) If it doesn't fire but the operator is a registered macro,
                  perform one-step expansion by registry.
               Guard with depth limit to avoid infinite loops. */
            for (int depth = 0; depth < 128; ++depth) {
                if (!expr || expr->type != VAL_PAIR) break;

                /* (1) hygienic */
                Syntax* s = syn_new(expr);
                Syntax* out = macroexpand1_hygienic(env, s);
                syn_free(s);
                if (out) {
                    expr = out->datum;
                    syn_free(out);
                    continue; /* try another pass */
                }

                /* (2) fallback by operator name */
                Value* op = car(expr);
                if (!(op && op->type == VAL_SYMBOL)) break;
                const SyntaxTransformer* tr = macro_lookup(op->sym);
                if (!tr) break;
                Syntax* s2 = syn_new(expr);
                Syntax* out2 = sr_expand_call(env, tr, s2);
                syn_free(s2);
                if (!out2) break;
                expr = out2->datum;
                syn_free(out2);
                /* continue; allow nested rewrites */
            }

                        /* If expansion changed the node to a non-pair (e.g., (and) -> #t), re-dispatch */
            if (!expr || expr->type != VAL_PAIR) {
                return eval(expr, env);
            }

            Value* head = car(expr);
            Value* args = cdr(expr);

            /* (quote x) */
            if (is_symbol(head, "quote")) {
                if (!ensure_arity_exact("quote", args, 1)) {
                    return make_nil();
                }
                return car(args) ? car(args) : make_nil();
            }

            /* (if test conseq [alt]) */
            if (is_symbol(head, "if")) {
                int __n_if = count_args(args);
                if (__n_if < 2) {
                    make_error("if: expected at least 2 arguments");
                    return make_nil();
                }
                Value* test   = car(args);
                Value* conseq = car(cdr(args));
                Value* alt    = (__n_if >= 3) ? car(cdr(cdr(args))) : make_nil();
                Value* tval = eval(test, env);
                if (is_truthy(tval)) {
                    return eval(conseq, env);
                }
                if (alt) {
                    return eval(alt, env);
                }
                return make_nil();
            }

            /* (defun name expr) */
            
                /* (define-syntax name spec) */
                if (is_symbol(head, "define-syntax")) {
                    if (!ensure_arity_exact("define-syntax", args, 2)) {
                        return make_nil();
                    }
                    Value* name = car(args);
                    Value* spec = car(cdr(args));
                    if (!ensure_symbol("define-syntax", name)) {
                        return make_nil();
                    }
                    /* Compile (syntax-rules ...) and register transformer */
                    Syntax* spec_syn = syn_new(spec);
                    SyntaxTransformer* tr = sr_compile(spec_syn);
                    syn_free(spec_syn);
                    if (!tr) {
                        make_error("define-syntax: invalid syntax-rules specification");
                        return make_nil();
                    }
                    macro_define(name->sym, tr);
                    return name;
                }
if (is_symbol(head, "defun")) {
                if (!ensure_arity_exact("defun", args, 2)) {
                    return make_nil();
                }
                Value* name = car(args);
                Value* rhs  = car(cdr(args));
                if (!ensure_symbol("defun", name)) {
                    return make_nil();
                }
                Value* val = eval(rhs, env);
                env_define(env, name->sym, val);
                return val;
            }

            /* (set! name expr) */
            if (is_symbol(head, "set!")) {
                if (!ensure_arity_exact("set!", args, 2)) {
                    return make_nil();
                }
                Value* name = car(args);
                Value* rhs  = car(cdr(args));
                if (!ensure_symbol("set!", name)) {
                    return make_nil();
                }
                Value* val = eval(rhs, env);
                if (!env_set(env, name->sym, val)) {
                    make_error("set!: unbound symbol");
                    return make_nil();
                }
                return val;
            }

            /* (begin e1 e2 ... en) */
            if (is_symbol(head, "begin")) {
                if (!args || args->type == VAL_NIL) {
                    make_error("begin: requires at least one expression");
                    return make_nil();
                }
                return eval_sequence(args, env);
            }

            /* (lambda (params...) body...) */
            if (is_symbol(head, "lambda")) {
                if (!args || args->type == VAL_NIL) {
                    make_error("lambda: requires parameter list and body");
                    return make_nil();
                }
                Value* params = car(args);
                Value* body   = cdr(args);
                if (!ensure_param_list_symbols("lambda", params)) {
                    return make_nil();
                }
                if (!body || body->type == VAL_NIL) {
                    make_error("lambda: body must have at least one form");
                    return make_nil();
                }
                Value* begin_sym = make_symbol("begin");
                Value* wrapped = cons(begin_sym, body);
                return make_closure(params, wrapped, env);
            }

            /* (and e1 e2 ... en) */
            if (is_symbol(head, "and")) {
                Value* last = make_bool(true);
                for (Value* it = args; it && it->type == VAL_PAIR; it = it->pair.cdr) {
                    last = eval(it->pair.car, env);
                    if (!is_truthy(last)) {
                        return last ? last : make_bool(false);
                    }
                }
                return last;
            }

            /* (or e1 e2 ... en) */
            if (is_symbol(head, "or")) {
                if (!args || args->type == VAL_NIL) {
                    return make_bool(false);
                }
                Value* last = make_bool(false);
                for (Value* it = args; it && it->type == VAL_PAIR; it = it->pair.cdr) {
                    Value* v = eval(it->pair.car, env);
                    if (is_truthy(v)) {
                        return v;
                    }
                    last = v;
                }
                return last ? last : make_bool(false);
            }

            /* (let ((x e1) (y e2) ...) body...)  — parallel bindings */
            if (is_symbol(head, "let")) {
                if (!args || args->type != VAL_PAIR) {
                    make_error("let: expected (let ((name expr) ...) body...)");
                    return make_nil();
                }
                Value* bindings = car(args);
                Value* body = cdr(args);
                if (!validate_binding_list(bindings, "let")) {
                    return make_nil();
                }
                int nbinds = list_length(bindings);
                Value** names = (Value**)malloc(sizeof(Value*) * (nbinds ? nbinds : 1));
                Value** values = (Value**)malloc(sizeof(Value*) * (nbinds ? nbinds : 1));
                if (!names || !values) {
                    fprintf(stderr, "OOM\n");
                    exit(1);
                }
                int i = 0;
                for (Value* it = bindings; it && it->type == VAL_PAIR; it = it->pair.cdr) {
                    Value* b = it->pair.car;
                    Value* name = car(b);
                    Value* exprv = car(cdr(b));
                    names[i] = name;
                    values[i] = eval(exprv, env);
                    i++;
                }
                Env* child = make_env(env);
                for (int j = 0; j < nbinds; ++j) {
                    env_define(child, names[j]->sym, values[j]);
                }
                free(names);
                free(values);
                if (!body || body->type == VAL_NIL) {
                    make_error("let: body must have at least one form");
                    return make_nil();
                }
                return eval_sequence(body, child);
            }

            /* (let* ((x e1) (y e2) ...) body...) — sequential bindings */
            if (is_symbol(head, "let*")) {
                if (!args || args->type != VAL_PAIR) {
                    make_error("let*: expected (let* ((name expr) ...) body...)");
                    return make_nil();
                }
                Value* bindings = car(args);
                Value* body = cdr(args);
                if (!validate_binding_list(bindings, "let*")) {
                    return make_nil();
                }
                Env* child = make_env(env);
                for (Value* it = bindings; it && it->type == VAL_PAIR; it = it->pair.cdr) {
                    Value* b = it->pair.car;
                    Value* name = car(b);
                    Value* exprv = car(cdr(b));
                    Value* val = eval(exprv, child);
                    env_define(child, name->sym, val);
                }
                if (!body || body->type == VAL_NIL) {
                    make_error("let*: body must have at least one form");
                    return make_nil();
                }
                return eval_sequence(body, child);
            }

            /* Function application */
            Value* fn = eval(head, env);
            Value* evargs = eval_list(args, env);
            return apply(fn, evargs, env);
        }
    }

    return make_nil();
}

/* Evaluate each item of a list and return a new list with results */
Value* eval_list(Value* list, Env* env) {
    if (!list || list->type == VAL_NIL) {
        return make_nil();
    }

    Value* out_head = NULL;
    Value* out_tail = NULL;

    for (Value* it = list; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        Value* v = eval(it->pair.car, env);
        Value* cell = cons(v, make_nil());
        if (!out_head) {
            out_head = out_tail = cell;
        } else {
            out_tail->pair.cdr = cell;
            out_tail = cell;
        }
    }

    if (!out_head) {
        out_head = make_nil();
    }
    return out_head;
}

/* Bind parameters to arguments in a new child environment */
static Env* bind_params(Value* params, Value* args, Env* parent) {
    Env* e = make_env(parent);

    for (;;) {
        bool params_done = (!params || params->type == VAL_NIL);
        bool args_done   = (!args   || args->type   == VAL_NIL);
        if (params_done && args_done) {
            break;
        }

        if (!params || params->type != VAL_PAIR) {
            fprintf(stderr, "lambda: invalid arity\n");
            break;
        }
        if (!args || args->type != VAL_PAIR) {
            fprintf(stderr, "lambda: not enough arguments\n");
            break;
        }

        Value* p = car(params);
        Value* a = car(args);
        if (!p || p->type != VAL_SYMBOL) {
            fprintf(stderr, "lambda: non-symbol parameter\n");
            break;
        }
        env_define(e, p->sym, a);

        params = cdr(params);
        args   = cdr(args);
    }

    return e;
}

/* Apply a builtin or closure */
Value* apply(Value* fn, Value* args, Env* env) {
    (void)env;

    if (!fn) {
        fprintf(stderr, "apply: null function\n");
        return make_nil();
    }

    switch (fn->type) {
        case VAL_BUILTIN: {
            return fn->builtin->fn(args, env);
        }
        case VAL_CLOSURE: {
            Env* call_env = bind_params(fn->clos->params, args, fn->clos->env);
            return eval(fn->clos->body, call_env);
        }
        default: {
            fprintf(stderr, "apply: object is not callable\n");
            return make_nil();
        }
    }
}
