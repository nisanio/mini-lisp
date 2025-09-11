#ifndef MINILISP_VALUE_H
#define MINILISP_VALUE_H

#include <stdbool.h>
#include <stddef.h>

struct Env;

typedef enum {
    VAL_NIL,
    VAL_INT,
    VAL_BOOL,
    VAL_SYMBOL,
    VAL_PAIR,
    VAL_BUILTIN,
    VAL_CLOSURE,
    VAL_STRING,
    VAL_VECTOR,
    VAL_HASH
} ValueType;

typedef struct Value   Value;
typedef struct Builtin Builtin;
typedef struct Closure Closure;

typedef Value* (*BuiltinFn)(Value* args, struct Env* env);

struct Builtin {
    const char* name;
    BuiltinFn fn;
};

struct Closure {
    Value* params;
    Value* body;
    struct Env* env;
};

struct Value {
    ValueType type;
    union {
        long i;                       /* VAL_INT */
        bool b;                       /* VAL_BOOL */
        const char* sym;              /* VAL_SYMBOL */
        struct { Value* car; Value* cdr; } pair; /* VAL_PAIR */
        Builtin* builtin;             /* VAL_BUILTIN */
        Closure* clos;                /* VAL_CLOSURE */
        struct {                      /* VAL_STRING */
            char* data;
            size_t len;
        } str;
        struct {                      /* VAL_VECTOR */
            Value** data;
            size_t len;
        } vec;
        struct {                      /* VAL_HASH */
            struct HashEntry* entries;
            size_t cap;
            size_t size;
        } h;
    };
};

typedef struct HashEntry {
    char* key;          /* owned C-string key */
    size_t hash;        /* cached hash */
    Value* val;         /* stored value */
    int used;           /* 0 = empty, 1 = used, 2 = tombstone */
} HashEntry;

/* Constructors */
Value* make_nil(void);
Value* make_int(long x);
Value* make_bool(bool b);
Value* make_symbol(const char* name);
Value* cons(Value* a, Value* d);
Value* make_builtin(BuiltinFn fn);
Value* make_closure(Value* params, Value* body, struct Env* env);

/* Strings */
Value* make_string_copy(const char* bytes, size_t len);
Value* make_string_cstr(const char* cstr);
Value* make_string_empty(void);

/* Vectors */
Value* make_vector_raw(size_t len);
Value* make_vector_filled(size_t len, Value* fill);
static inline size_t vector_length(Value* v) { return (v && v->type == VAL_VECTOR) ? v->vec.len : 0; }

/* Hash tables */
Value* make_hash(size_t initial_cap);       /* cap rounded up to power-of-two >= 8 */
size_t hash_size(Value* h);
Value* hash_get_cstr(Value* h, const char* key, Value* defaultv);
void   hash_set_cstr(Value* h, const char* key, Value* v);
int    hash_has_key_cstr(Value* h, const char* key);
int    hash_remove_cstr(Value* h, const char* key);

/* List helpers */
static inline Value* car(Value* p) { return (p && p->type == VAL_PAIR) ? p->pair.car : NULL; }
static inline Value* cdr(Value* p) { return (p && p->type == VAL_PAIR) ? p->pair.cdr : NULL; }
Value* list_from_array(Value** items, int n);
int    list_length(Value* lst);

/* Truth and printing */
bool   is_truthy(Value* v);
void   print_value(Value* v);

/* Symbol helper */
bool   is_symbol(Value* v, const char* name);

#endif /* MINILISP_VALUE_H */
