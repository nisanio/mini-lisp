#include "macros.h"
#include "syntax.h"
#include "builtins.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* Prototypes for macro expansion builtins */
static Value* builtin_macroexpand1(Value* args, Env* env);
static Value* builtin_macroexpand(Value* args, Env* env);



/* Helpers */

static long expect_int(Value* v, const char* who, int* ok) {
    if (!v || v->type != VAL_INT) {
        fprintf(stderr, "%s: expected integer\n", who);
        if (ok) {
            *ok = 0;
        }
        return 0;
    }
    if (ok) {
        *ok = 1;
    }
    return v->i;
}

static Value* boolv(int cond) {
    return make_bool(cond ? true : false);
}

/* Check proper list; return length via *len if not NULL */
static int is_proper_list(Value* v, int* len) {
    int n = 0;
    while (v && v->type == VAL_PAIR) {
        n++;
        v = v->pair.cdr;
    }
    if (v && v->type != VAL_NIL) {
        return 0;
    }
    if (len) {
        *len = n;
    }
    return 1;
}

/* Deep structural equality for equal? (includes strings; vectors/hash by identity) */
static int equal_deep(Value* a, Value* b) {
    if (a == b) {
        return 1;
    }
    if (!a || !b) {
        return 0;
    }
    if (a->type != b->type) {
        if (a->type == VAL_INT && b->type == VAL_INT) {
            return a->i == b->i;
        }
        return 0;
    }
    switch (a->type) {
        case VAL_NIL: {
            return 1;
        }
        case VAL_INT: {
            return a->i == b->i;
        }
        case VAL_BOOL: {
            return a->b == b->b;
        }
        case VAL_SYMBOL: {
            return (a->sym && b->sym) ? (strcmp(a->sym, b->sym) == 0) : (a->sym == b->sym);
        }
        case VAL_STRING: {
            size_t la = a->str.len, lb = b->str.len;
            if (la != lb) {
                return 0;
            }
            if (la == 0) {
                return 1;
            }
            return memcmp(a->str.data, b->str.data, la) == 0;
        }
        case VAL_BUILTIN:
        case VAL_CLOSURE:
        case VAL_VECTOR:
        case VAL_HASH: {
            return a == b; /* identity for aggregates */
        }
        case VAL_PAIR: {
            return equal_deep(a->pair.car, b->pair.car) && equal_deep(a->pair.cdr, b->pair.cdr);
        }
    }
    return 0;
}

/* Arithmetic */

static Value* builtin_add(Value* args, Env* env) {
    (void)env;
    long acc = 0;
    int ok = 1;
    for (Value* it = args; ok && it && it->type == VAL_PAIR; it = it->pair.cdr) {
        acc += expect_int(it->pair.car, "+", &ok);
    }
    return ok ? make_int(acc) : make_int(0);
}

static Value* builtin_sub(Value* args, Env* env) {
    (void)env;
    if (!args || args->type != VAL_PAIR) {
        return make_int(0);
    }
    int ok = 1;
    long acc = expect_int(car(args), "-", &ok);
    if (!ok) {
        return make_int(0);
    }
    Value* it = cdr(args);
    if (!it || it->type != VAL_PAIR) {
        return make_int(-acc);
    }
    for (; ok && it && it->type == VAL_PAIR; it = it->pair.cdr) {
        acc -= expect_int(it->pair.car, "-", &ok);
    }
    return ok ? make_int(acc) : make_int(0);
}

static Value* builtin_mul(Value* args, Env* env) {
    (void)env;
    long acc = 1;
    int ok = 1;
    for (Value* it = args; ok && it && it->type == VAL_PAIR; it = it->pair.cdr) {
        acc *= expect_int(it->pair.car, "*", &ok);
    }
    return ok ? make_int(acc) : make_int(0);
}

static Value* builtin_div(Value* args, Env* env) {
    (void)env;
    if (!args || args->type != VAL_PAIR) {
        fprintf(stderr, "/: needs at least one argument\n");
        return make_int(0);
    }
    int ok = 1;
    long acc = expect_int(car(args), "/", &ok);
    if (!ok) {
        return make_int(0);
    }
    for (Value* it = cdr(args); ok && it && it->type == VAL_PAIR; it = it->pair.cdr) {
        long d = expect_int(it->pair.car, "/", &ok);
        if (!ok) {
            return make_int(0);
        }
        if (d == 0) {
            fprintf(stderr, "/: division by zero\n");
            return make_int(0);
        }
        acc /= d;
    }
    return make_int(acc);
}

static Value* builtin_mod(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "%%: expected 2 arguments\n");
        return make_int(0);
    }
    int ok1 = 1, ok2 = 1;
    long a = expect_int(car(args), "%", &ok1);
    long b = expect_int(car(cdr(args)), "%", &ok2);
    if (!ok1 || !ok2) {
        return make_int(0);
    }
    if (b == 0) {
        fprintf(stderr, "%%: division by zero\n");
        return make_int(0);
    }
    return make_int(a % b);
}

static Value* builtin_abs(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "abs: expected 1 argument\n");
        return make_int(0);
    }
    int ok = 1;
    long a = expect_int(car(args), "abs", &ok);
    if (!ok) {
        return make_int(0);
    }
    return make_int(labs(a));
}

static Value* builtin_max(Value* args, Env* env) {
    (void)env;
    if (!args || args->type != VAL_PAIR) {
        fprintf(stderr, "max: expected at least 1 argument\n");
        return make_int(0);
    }
    int ok = 1;
    long m = expect_int(car(args), "max", &ok);
    if (!ok) {
        return make_int(0);
    }
    for (Value* it = cdr(args); it && it->type == VAL_PAIR; it = it->pair.cdr) {
        int ok2 = 1;
        long v = expect_int(it->pair.car, "max", &ok2);
        if (!ok2) {
            return make_int(0);
        }
        if (v > m) {
            m = v;
        }
    }
    return make_int(m);
}

static Value* builtin_min(Value* args, Env* env) {
    (void)env;
    if (!args || args->type != VAL_PAIR) {
        fprintf(stderr, "min: expected at least 1 argument\n");
        return make_int(0);
    }
    int ok = 1;
    long m = expect_int(car(args), "min", &ok);
    if (!ok) {
        return make_int(0);
    }
    for (Value* it = cdr(args); it && it->type == VAL_PAIR; it = it->pair.cdr) {
        int ok2 = 1;
        long v = expect_int(it->pair.car, "min", &ok2);
        if (!ok2) {
            return make_int(0);
        }
        if (v < m) {
            m = v;
        }
    }
    return make_int(m);
}

/* Comparison / Equality */

static int fold_chain(Value* args, const char* who, int (*cmp)(long,long)) {
    if (!args || args->type == VAL_NIL) {
        return 1;
    }
    if (args->type != VAL_PAIR) {
        return 0;
    }
    int ok = 1;
    long prev = expect_int(car(args), who, &ok);
    if (!ok) {
        return 0;
    }
    Value* it = cdr(args);
    if (!it || it->type == VAL_NIL) {
        return 1;
    }
    for (; ok && it && it->type == VAL_PAIR; it = it->pair.cdr) {
        int ok2 = 1;
        long cur = expect_int(it->pair.car, who, &ok2);
        if (!ok2) {
            ok = 0;
            break;
        }
        if (!cmp(prev, cur)) {
            return 0;
        }
        prev = cur;
    }
    return ok ? 1 : 0;
}

static int cmp_eq(long a, long b) { return a == b; }
static int cmp_lt(long a, long b) { return a <  b; }
static int cmp_gt(long a, long b) { return a >  b; }
static int cmp_le(long a, long b) { return a <= b; }
static int cmp_ge(long a, long b) { return a >= b; }

static Value* builtin_num_eq(Value* args, Env* env) { (void)env; return boolv(fold_chain(args, "=",  cmp_eq)); }
static Value* builtin_lt    (Value* args, Env* env) { (void)env; return boolv(fold_chain(args, "<",  cmp_lt)); }
static Value* builtin_gt    (Value* args, Env* env) { (void)env; return boolv(fold_chain(args, ">",  cmp_gt)); }
static Value* builtin_le    (Value* args, Env* env) { (void)env; return boolv(fold_chain(args, "<=", cmp_le)); }
static Value* builtin_ge    (Value* args, Env* env) { (void)env; return boolv(fold_chain(args, ">=", cmp_ge)); }

/* eq? — identity for non-ints; numeric equality for ints */
static Value* builtin_eqp(Value* args, Env* env) {
    (void)env;
    int n = list_length(args);
    if (n != 2) {
        fprintf(stderr, "eq?: expected 2 arguments, got %d\n", n);
        return make_bool(false);
    }
    Value* a = car(args);
    Value* b = car(cdr(args));
    if (!a || !b) {
        return make_bool(false);
    }
    if (a->type == VAL_INT && b->type == VAL_INT) {
        return make_bool(a->i == b->i);
    }
    return make_bool(a == b);
}

/* equal? — deep structural equality (pairs, strings) */
static Value* builtin_equalp(Value* args, Env* env) {
    (void)env;
    int n = list_length(args);
    if (n != 2) {
        fprintf(stderr, "equal?: expected 2 arguments, got %d\n", n);
        return make_bool(false);
    }
    Value* a = car(args);
    Value* b = car(cdr(args));
    return make_bool(equal_deep(a, b));
}

/* not — true iff arg is falsey */
static Value* builtin_not(Value* args, Env* env) {
    (void)env;
    int n = list_length(args);
    if (n != 1) {
        fprintf(stderr, "not: expected 1 argument, got %d\n", n);
        return make_bool(false);
    }
    return make_bool(!is_truthy(car(args)));
}

/* Lists */

static Value* builtin_cons(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "cons: expected 2 arguments\n");
        return make_nil();
    }
    Value* a = car(args);
    Value* d = car(cdr(args));
    return cons(a, d);
}

static Value* builtin_car(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "car: expected 1 argument\n");
        return make_nil();
    }
    Value* p = car(args);
    if (!p || p->type != VAL_PAIR) {
        fprintf(stderr, "car: expected pair\n");
        return make_nil();
    }
    return p->pair.car ? p->pair.car : make_nil();
}

static Value* builtin_cdr(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "cdr: expected 1 argument\n");
        return make_nil();
    }
    Value* p = car(args);
    if (!p || p->type != VAL_PAIR) {
        fprintf(stderr, "cdr: expected pair\n");
        return make_nil();
    }
    return p->pair.cdr ? p->pair.cdr : make_nil();
}

static Value* builtin_list(Value* args, Env* env) {
    (void)env;
    return args ? args : make_nil();
}

static Value* builtin_nullp(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "null?: expected 1 argument\n");
        return make_bool(false);
    }
    Value* v = car(args);
    return make_bool(!v || v->type == VAL_NIL);
}

static Value* builtin_length(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "length: expected 1 argument\n");
        return make_int(0);
    }
    Value* v = car(args);
    int n = 0;
    if (!is_proper_list(v, &n)) {
        fprintf(stderr, "length: improper list\n");
        return make_int(0);
    }
    return make_int(n);
}

/* IO */

static Value* builtin_print(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "print: expected 1 argument\n");
        return make_nil();
    }
    Value* v = car(args);
    print_value(v);
    printf("\n");
    return v ? v : make_nil();
}

/* Strings */

static int cmp_strings(Value* a, Value* b) {
    size_t la = a->str.len, lb = b->str.len;
    size_t n = (la < lb) ? la : lb;
    int c = (n ? memcmp(a->str.data, b->str.data, n) : 0);
    if (c == 0) {
        if (la < lb) {
            return -1;
        }
        if (la > lb) {
            return 1;
        }
        return 0;
    }
    return c;
}

static int all_strings(Value* args) {
    for (Value* it = args; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        Value* v = it->pair.car;
        if (!v || v->type != VAL_STRING) {
            return 0;
        }
    }
    return 1;
}

static Value* string_chain(Value* args, int rel) {
    if (!args || args->type == VAL_NIL || !args->pair.cdr || args->pair.cdr->type == VAL_NIL) {
        return make_bool(true);
    }
    if (!all_strings(args)) {
        return make_bool(false);
    }
    Value* it = args;
    Value* a  = it->pair.car;
    it = it->pair.cdr;
    while (it && it->type == VAL_PAIR) {
        Value* b = it->pair.car;
        int c = cmp_strings(a, b);
        int ok = 0;
        switch (rel) {
            case 0: { ok = (c == 0); } break;
            case 1: { ok = (c <  0); } break;
            case 2: { ok = (c >  0); } break;
            case 3: { ok = (c <= 0); } break;
            case 4: { ok = (c >= 0); } break;
        }
        if (!ok) {
            return make_bool(false);
        }
        a = b;
        it = it->pair.cdr;
    }
    return make_bool(true);
}

static Value* builtin_string_length(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "string-length: expected 1 argument\n");
        return make_int(0);
    }
    Value* s = car(args);
    if (!s || s->type != VAL_STRING) {
        fprintf(stderr, "string-length: expected string\n");
        return make_int(0);
    }
    return make_int((long)s->str.len);
}

static Value* builtin_string_append(Value* args, Env* env) {
    (void)env;
    size_t total = 0;
    for (Value* it = args; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        Value* s = it->pair.car;
        if (!s || s->type != VAL_STRING) {
            fprintf(stderr, "string-append: expected string\n");
            return make_string_empty();
        }
        total += s->str.len;
    }
    char* buf = (char*)malloc(total + 1);
    if (!buf) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    size_t pos = 0;
    for (Value* it = args; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        Value* s = it->pair.car;
        memcpy(buf + pos, s->str.data, s->str.len);
        pos += s->str.len;
    }
    buf[pos] = '\0';
    Value* v = make_string_copy(buf, pos);
    free(buf);
    return v;
}

static Value* builtin_substring(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 3) {
        fprintf(stderr, "substring: expected 3 arguments\n");
        return make_string_empty();
    }
    Value* s = car(args);
    Value* a = car(cdr(args));
    Value* b = car(cdr(cdr(args)));
    if (!s || s->type != VAL_STRING) {
        fprintf(stderr, "substring: expected string\n");
        return make_string_empty();
    }
    if (!a || a->type != VAL_INT || !b || b->type != VAL_INT) {
        fprintf(stderr, "substring: expected ints\n");
        return make_string_empty();
    }
    long start = a->i;
    long end   = b->i;
    if (start < 0) {
        start = 0;
    }
    if (end > (long)s->str.len) {
        end = (long)s->str.len;
    }
    if (end < start) {
        end = start;
    }
    return make_string_copy(s->str.data + start, (size_t)(end - start));
}

static Value* builtin_string_ref(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "string-ref: expected 2 arguments\n");
        return make_int(0);
    }
    Value* s = car(args);
    Value* i = car(cdr(args));
    if (!s || s->type != VAL_STRING) {
        fprintf(stderr, "string-ref: expected string\n");
        return make_int(0);
    }
    if (!i || i->type != VAL_INT) {
        fprintf(stderr, "string-ref: expected int\n");
        return make_int(0);
    }
    long idx = i->i;
    if (idx < 0 || idx >= (long)s->str.len) {
        fprintf(stderr, "string-ref: index out of range\n");
        return make_int(0);
    }
    return make_int((unsigned char)s->str.data[idx]);
}

static Value* builtin_string_eq(Value* args, Env* env) { (void)env; return string_chain(args, 0); }
static Value* builtin_string_lt(Value* args, Env* env) { (void)env; return string_chain(args, 1); }
static Value* builtin_string_gt(Value* args, Env* env) { (void)env; return string_chain(args, 2); }
static Value* builtin_string_le(Value* args, Env* env) { (void)env; return string_chain(args, 3); }
static Value* builtin_string_ge(Value* args, Env* env) { (void)env; return string_chain(args, 4); }

/* Vectors */

static Value* builtin_make_vector(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "make-vector: expected 2 arguments\n");
        return make_vector_raw(0);
    }
    int ok = 1;
    long n = expect_int(car(args), "make-vector", &ok);
    if (!ok || n < 0) {
        fprintf(stderr, "make-vector: size must be non-negative integer\n");
        return make_vector_raw(0);
    }
    Value* fill = car(cdr(args));
    return make_vector_filled((size_t)n, fill);
}

static Value* builtin_vector(Value* args, Env* env) {
    (void)env;
    int n = list_length(args);
    if (n <= 0) {
        return make_vector_raw(0);
    }
    Value* v = make_vector_raw((size_t)n);
    int i = 0;
    for (Value* it = args; it && it->type == VAL_PAIR; it = it->pair.cdr) {
        v->vec.data[i++] = it->pair.car;
    }
    return v;
}

static Value* builtin_vector_length(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "vector-length: expected 1 argument\n");
        return make_int(0);
    }
    Value* v = car(args);
    if (!v || v->type != VAL_VECTOR) {
        fprintf(stderr, "vector-length: expected vector\n");
        return make_int(0);
    }
    return make_int((long)v->vec.len);
}

static Value* builtin_vector_ref(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "vector-ref: expected 2 arguments\n");
        return make_nil();
    }
    Value* v = car(args);
    Value* i = car(cdr(args));
    if (!v || v->type != VAL_VECTOR) {
        fprintf(stderr, "vector-ref: expected vector\n");
        return make_nil();
    }
    if (!i || i->type != VAL_INT) {
        fprintf(stderr, "vector-ref: expected int\n");
        return make_nil();
    }
    long idx = i->i;
    if (idx < 0 || idx >= (long)v->vec.len) {
        fprintf(stderr, "vector-ref: index out of range\n");
        return make_nil();
    }
    Value* elem = v->vec.data[(size_t)idx];
    return elem ? elem : make_nil();
}

static Value* builtin_vector_set(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 3) {
        fprintf(stderr, "vector-set!: expected 3 arguments\n");
        return make_nil();
    }
    Value* v = car(args);
    Value* i = car(cdr(args));
    Value* x = car(cdr(cdr(args)));
    if (!v || v->type != VAL_VECTOR) {
        fprintf(stderr, "vector-set!: expected vector\n");
        return make_nil();
    }
    if (!i || i->type != VAL_INT) {
        fprintf(stderr, "vector-set!: expected int index\n");
        return make_nil();
    }
    long idx = i->i;
    if (idx < 0 || idx >= (long)v->vec.len) {
        fprintf(stderr, "vector-set!: index out of range\n");
        return make_nil();
    }
    v->vec.data[(size_t)idx] = x;
    return v;
}

/* Hash tables */

static int ensure_hash(const char* who, Value* v) {
    if (!v || v->type != VAL_HASH) {
        fprintf(stderr, "%s: expected hash\n", who);
        return 0;
    }
    return 1;
}

static int ensure_string_key(const char* who, Value* v) {
    if (!v || v->type != VAL_STRING) {
        fprintf(stderr, "%s: expected string key\n", who);
        return 0;
    }
    return 1;
}

static Value* builtin_make_hash(Value* args, Env* env) {
    (void)env;
    int n = list_length(args);
    if (n == 0) {
        return make_hash(8);
    }
    if (n == 1) {
        int ok = 1;
        long cap = expect_int(car(args), "make-hash", &ok);
        if (!ok || cap < 0) {
            fprintf(stderr, "make-hash: capacity must be non-negative integer\n");
            return make_hash(8);
        }
        return make_hash((size_t)cap);
    }
    fprintf(stderr, "make-hash: expected 0 or 1 argument(s)\n");
    return make_hash(8);
}

static Value* builtin_hash_size(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "hash-size: expected 1 argument\n");
        return make_int(0);
    }
    Value* h = car(args);
    if (!ensure_hash("hash-size", h)) {
        return make_int(0);
    }
    return make_int((long)hash_size(h));
}

static Value* builtin_hash_has_key(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "hash-has-key?: expected 2 arguments\n");
        return make_bool(false);
    }
    Value* h = car(args);
    Value* k = car(cdr(args));
    if (!ensure_hash("hash-has-key?", h) || !ensure_string_key("hash-has-key?", k)) {
        return make_bool(false);
    }
    return make_bool(hash_has_key_cstr(h, k->str.data));
}

static Value* builtin_hash_ref(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 3) {
        fprintf(stderr, "hash-ref: expected 3 arguments\n");
        return make_nil();
    }
    Value* h = car(args);
    Value* k = car(cdr(args));
    Value* d = car(cdr(cdr(args)));
    if (!ensure_hash("hash-ref", h) || !ensure_string_key("hash-ref", k)) {
        return make_nil();
    }
    return hash_get_cstr(h, k->str.data, d);
}

static Value* builtin_hash_set(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 3) {
        fprintf(stderr, "hash-set!: expected 3 arguments\n");
        return make_nil();
    }
    Value* h = car(args);
    Value* k = car(cdr(args));
    Value* v = car(cdr(cdr(args)));
    if (!ensure_hash("hash-set!", h) || !ensure_string_key("hash-set!", k)) {
        return make_nil();
    }
    hash_set_cstr(h, k->str.data, v);
    return h;
}

static Value* builtin_hash_remove(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "hash-remove!: expected 2 arguments\n");
        return make_bool(false);
    }
    Value* h = car(args);
    Value* k = car(cdr(args));
    if (!ensure_hash("hash-remove!", h) || !ensure_string_key("hash-remove!", k)) {
        return make_bool(false);
    }
    return make_bool(hash_remove_cstr(h, k->str.data));
}

/* ----------------------------
   File I/O builtins
   ---------------------------- */

static Value* builtin_read_file(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "read-file: expected 1 argument\n");
        return make_string_empty();
    }
    Value* pathv = car(args);
    if (!pathv || pathv->type != VAL_STRING) {
        fprintf(stderr, "read-file: expected string path\n");
        return make_string_empty();
    }
    const char* path = pathv->str.data;
    FILE* f = fopen(path, "rb");
    if (!f) {
        return make_string_empty();
    }
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (size < 0) size = 0;
    char* buf = (char*)malloc((size_t)size + 1);
    if (!buf) {
        fclose(f);
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    size_t nread = fread(buf, 1, (size_t)size, f);
    buf[nread] = '\0';
    fclose(f);
    Value* v = make_string_copy(buf, nread);
    free(buf);
    return v;
}

static Value* builtin_write_file(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "write-file: expected 2 arguments\n");
        return make_bool(false);
    }
    Value* pathv = car(args);
    Value* content = car(cdr(args));
    if (!pathv || pathv->type != VAL_STRING || !content || content->type != VAL_STRING) {
        fprintf(stderr, "write-file: expected (string path, string content)\n");
        return make_bool(false);
    }
    FILE* f = fopen(pathv->str.data, "wb");
    if (!f) {
        return make_bool(false);
    }
    size_t nw = fwrite(content->str.data, 1, content->str.len, f);
    fclose(f);
    return make_bool(nw == content->str.len);
}

static Value* builtin_append_file(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 2) {
        fprintf(stderr, "append-file: expected 2 arguments\n");
        return make_bool(false);
    }
    Value* pathv = car(args);
    Value* content = car(cdr(args));
    if (!pathv || pathv->type != VAL_STRING || !content || content->type != VAL_STRING) {
        fprintf(stderr, "append-file: expected (string path, string content)\n");
        return make_bool(false);
    }
    FILE* f = fopen(pathv->str.data, "ab");
    if (!f) {
        return make_bool(false);
    }
    size_t nw = fwrite(content->str.data, 1, content->str.len, f);
    fclose(f);
    return make_bool(nw == content->str.len);
}

static Value* builtin_file_exists(Value* args, Env* env) {
    (void)env;
    if (list_length(args) != 1) {
        fprintf(stderr, "file-exists?: expected 1 argument\n");
        return make_bool(false);
    }
    Value* pathv = car(args);
    if (!pathv || pathv->type != VAL_STRING) {
        fprintf(stderr, "file-exists?: expected string path\n");
        return make_bool(false);
    }
    FILE* f = fopen(pathv->str.data, "rb");
    if (!f) {
        return make_bool(false);
    }
    fclose(f);
    return make_bool(true);
}

/* Installation */


static Value* builtin_macroexpand1(Value* args, Env* env) {
    (void)env;
    if (!args || cdr(args) != make_nil()) {
        fprintf(stderr, "macroexpand-1: expected 1 argument\n");
        return make_nil();
    }
    Value* form = car(args);
    Syntax* s = syn_new(form);
    Syntax* out = macroexpand1_hygienic(env, s);
    syn_free(s);
    if (!out) return form; /* not a macro call -> return input */
    Value* v = out->datum;
    syn_free(out);
    return v;
}

static Value* builtin_macroexpand(Value* args, Env* env) {
    (void)env;
    if (!args || cdr(args) != make_nil()) {
        fprintf(stderr, "macroexpand: expected 1 argument\n");
        return make_nil();
    }
    Value* form = car(args);
    for (int i=0; i<128; ++i) { /* depth limit to avoid infinite loops */
        Syntax* s = syn_new(form);
        Syntax* out = macroexpand1_hygienic(env, s);
        syn_free(s);
        if (!out) break;
        form = out->datum;
        syn_free(out);
    }
    return form;
}
void install_builtins(Env* env) {
    /* Macro expansion tools */
    env_define(env, "macroexpand-1", make_builtin(builtin_macroexpand1));
    env_define(env, "macroexpand",   make_builtin(builtin_macroexpand));

    /* Arithmetic */
    env_define(env, "+",   make_builtin(builtin_add));
    env_define(env, "-",   make_builtin(builtin_sub));
    env_define(env, "*",   make_builtin(builtin_mul));
    env_define(env, "/",   make_builtin(builtin_div));
    env_define(env, "%",   make_builtin(builtin_mod));
    env_define(env, "abs", make_builtin(builtin_abs));
    env_define(env, "max", make_builtin(builtin_max));
    env_define(env, "min", make_builtin(builtin_min));

    /* Comparison / equality */
    env_define(env, "=",       make_builtin(builtin_num_eq));
    env_define(env, "<",       make_builtin(builtin_lt));
    env_define(env, ">",       make_builtin(builtin_gt));
    env_define(env, "<=",      make_builtin(builtin_le));
    env_define(env, ">=",      make_builtin(builtin_ge));
    env_define(env, "eq?",     make_builtin(builtin_eqp));
    env_define(env, "equal?",  make_builtin(builtin_equalp));
    env_define(env, "not",     make_builtin(builtin_not));

    /* Lists */
    env_define(env, "cons",    make_builtin(builtin_cons));
    env_define(env, "car",     make_builtin(builtin_car));
    env_define(env, "cdr",     make_builtin(builtin_cdr));
    env_define(env, "list",    make_builtin(builtin_list));
    env_define(env, "null?",   make_builtin(builtin_nullp));
    env_define(env, "length",  make_builtin(builtin_length));

    /* IO */
    env_define(env, "print",   make_builtin(builtin_print));

    /* Strings */
    env_define(env, "string-length", make_builtin(builtin_string_length));
    env_define(env, "string-append", make_builtin(builtin_string_append));
    env_define(env, "substring",     make_builtin(builtin_substring));
    env_define(env, "string-ref",    make_builtin(builtin_string_ref));
    env_define(env, "string=?",      make_builtin(builtin_string_eq));
    env_define(env, "string<?",      make_builtin(builtin_string_lt));
    env_define(env, "string>?",      make_builtin(builtin_string_gt));
    env_define(env, "string<=?",     make_builtin(builtin_string_le));
    env_define(env, "string>=?",     make_builtin(builtin_string_ge));

    /* Vectors */
    env_define(env, "make-vector",   make_builtin(builtin_make_vector));
    env_define(env, "vector",        make_builtin(builtin_vector));
    env_define(env, "vector-length", make_builtin(builtin_vector_length));
    env_define(env, "vector-ref",    make_builtin(builtin_vector_ref));
    env_define(env, "vector-set!",   make_builtin(builtin_vector_set));

    /* Hash tables */
    env_define(env, "make-hash",     make_builtin(builtin_make_hash));
    env_define(env, "hash-size",     make_builtin(builtin_hash_size));
    env_define(env, "hash-has-key?", make_builtin(builtin_hash_has_key));
    env_define(env, "hash-ref",      make_builtin(builtin_hash_ref));
    env_define(env, "hash-set!",     make_builtin(builtin_hash_set));
    env_define(env, "hash-remove!",  make_builtin(builtin_hash_remove));

    /* File I/O */
    env_define(env, "read-file",    make_builtin(builtin_read_file));
    env_define(env, "write-file",   make_builtin(builtin_write_file));
    env_define(env, "append-file",  make_builtin(builtin_append_file));
    env_define(env, "file-exists?", make_builtin(builtin_file_exists));

}
