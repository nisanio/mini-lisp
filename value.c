#include "value.h"
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


/* Core constructors */

Value* make_nil(void) {
    static Value the_nil = { .type = VAL_NIL };
    return &the_nil;
}

Value* make_int(long x) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_INT;
    v->i = x;
    return v;
}

Value* make_bool(bool b) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_BOOL;
    v->b = b;
    return v;
}

Value* make_symbol(const char* name) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_SYMBOL;
    v->sym = xstrdup(name);
    if (!v->sym) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    return v;
}

Value* cons(Value* a, Value* d) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_PAIR;
    v->pair.car = a;
    v->pair.cdr = d;
    return v;
}

Value* make_builtin(BuiltinFn fn) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_BUILTIN;
    v->builtin = (Builtin*)malloc(sizeof(Builtin));
    if (!v->builtin) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->builtin->name = NULL;
    v->builtin->fn = fn;
    return v;
}

Value* make_closure(Value* params, Value* body, struct Env* env) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_CLOSURE;
    v->clos = (Closure*)malloc(sizeof(Closure));
    if (!v->clos) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->clos->params = params;
    v->clos->body = body;
    v->clos->env = env;
    return v;
}

/* String constructors */

Value* make_string_copy(const char* bytes, size_t len) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_STRING;
    v->str.data = (char*)malloc(len + 1);
    if (!v->str.data) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    if (len > 0) {
        memcpy(v->str.data, bytes, len);
    }
    v->str.data[len] = '\0';
    v->str.len = len;
    return v;
}

Value* make_string_cstr(const char* cstr) {
    if (!cstr) {
        cstr = "";
    }
    return make_string_copy(cstr, strlen(cstr));
}

Value* make_string_empty(void) {
    return make_string_copy("", 0);
}

/* Vector constructors */

Value* make_vector_raw(size_t len) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_VECTOR;
    v->vec.len = len;
    v->vec.data = (Value**)calloc(len ? len : 1, sizeof(Value*));
    if (!v->vec.data) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    for (size_t i = 0; i < len; ++i) {
        v->vec.data[i] = make_nil();
    }
    return v;
}

Value* make_vector_filled(size_t len, Value* fill) {
    Value* v = make_vector_raw(len);
    for (size_t i = 0; i < len; ++i) {
        v->vec.data[i] = fill;
    }
    return v;
}

/* Hash table implementation (open addressing, FNV-1a) */

static size_t next_pow2(size_t x) {
    if (x < 8) {
        return 8;
    }
    x--;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
#if SIZE_MAX > 0xffffffffu
    x |= x >> 32;
#endif
    x++;
    return x;
}

static size_t fnv1a(const char* s) {
    /* 64-bit FNV-1a */
    const unsigned long long FNV_OFFSET = 1469598103934665603ull;
    const unsigned long long FNV_PRIME  = 1099511628211ull;
    unsigned long long h = FNV_OFFSET;
    for (const unsigned char* p = (const unsigned char*)s; *p; ++p) {
        h ^= (unsigned long long)(*p);
        h *= FNV_PRIME;
    }
    return (size_t)h;
}

static void hash_alloc_entries(Value* h, size_t cap) {
    h->h.entries = (HashEntry*)calloc(cap, sizeof(HashEntry));
    if (!h->h.entries) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    h->h.cap = cap;
    h->h.size = 0;
}

Value* make_hash(size_t initial_cap) {
    Value* v = (Value*)malloc(sizeof(Value));
    if (!v) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    v->type = VAL_HASH;
    size_t cap = next_pow2(initial_cap);
    hash_alloc_entries(v, cap);
    return v;
}

size_t hash_size(Value* h) {
    if (!h || h->type != VAL_HASH) {
        return 0;
    }
    return h->h.size;
}

static int key_equal(const char* a, const char* b) {
    if (a == b) {
        return 1;
    }
    if (!a || !b) {
        return 0;
    }
    return strcmp(a, b) == 0;
}

static void hash_grow(Value* h);

static size_t probe_index(Value* h, const char* key, size_t hash, int* found, size_t* tomb) {
    size_t mask = h->h.cap - 1;
    size_t i = hash & mask;
    *found = 0;
    *tomb = (size_t)(-1);
    for (;;) {
        HashEntry* e = &h->h.entries[i];
        if (e->used == 0) {
            if (*tomb != (size_t)(-1)) {
                return *tomb;
            }
            return i;
        } else if (e->used == 2) {
            if (*tomb == (size_t)(-1)) {
                *tomb = i;
            }
        } else {
            if (e->hash == hash && key_equal(e->key, key)) {
                *found = 1;
                return i;
            }
        }
        i = (i + 1) & mask;
    }
}

static void hash_insert_entry(Value* h, const char* key, size_t hash, Value* val) {
    int found;
    size_t tomb;
    size_t idx = probe_index(h, key, hash, &found, &tomb);
    HashEntry* e = &h->h.entries[idx];
    if (found) {
        e->val = val;
        return;
    }
    if (e->used != 1) {
        e->key = xstrdup(key);
        if (!e->key) {
            fprintf(stderr, "OOM\n");
            exit(1);
        }
        e->hash = hash;
        e->val = val;
        e->used = 1;
        h->h.size += 1;
    } else {
        /* Should not happen */
        e->val = val;
    }
    /* Grow when load factor > 0.7 (size >= 0.7 * cap) */
    if ((h->h.size * 10) >= (h->h.cap * 7)) {
        hash_grow(h);
    }
}

static void hash_reinsert_all(Value* h, HashEntry* old, size_t oldcap) {
    for (size_t i = 0; i < oldcap; ++i) {
        if (old[i].used == 1) {
            hash_insert_entry(h, old[i].key, old[i].hash, old[i].val);
            free(old[i].key);
        }
    }
}

static void hash_grow(Value* h) {
    size_t newcap = h->h.cap * 2;
    HashEntry* old = h->h.entries;
    size_t oldcap = h->h.cap;
    hash_alloc_entries(h, newcap);
    hash_reinsert_all(h, old, oldcap);
    free(old);
}

Value* hash_get_cstr(Value* h, const char* key, Value* defaultv) {
    if (!h || h->type != VAL_HASH) {
        return defaultv;
    }
    size_t hash = fnv1a(key);
    int found;
    size_t tomb;
    size_t idx = probe_index(h, key, hash, &found, &tomb);
    if (!found) {
        return defaultv;
    }
    return h->h.entries[idx].val ? h->h.entries[idx].val : make_nil();
}

void hash_set_cstr(Value* h, const char* key, Value* v) {
    if (!h || h->type != VAL_HASH) {
        return;
    }
    size_t hash = fnv1a(key);
    hash_insert_entry(h, key, hash, v);
}

int hash_has_key_cstr(Value* h, const char* key) {
    if (!h || h->type != VAL_HASH) {
        return 0;
    }
    size_t hash = fnv1a(key);
    int found;
    size_t tomb;
    probe_index(h, key, hash, &found, &tomb);
    return found ? 1 : 0;
}

int hash_remove_cstr(Value* h, const char* key) {
    if (!h || h->type != VAL_HASH) {
        return 0;
    }
    size_t hash = fnv1a(key);
    int found;
    size_t tomb;
    size_t idx = probe_index(h, key, hash, &found, &tomb);
    if (!found) {
        return 0;
    }
    HashEntry* e = &h->h.entries[idx];
    if (e->used == 1) {
        e->used = 2; /* tombstone */
        free(e->key);
        e->key = NULL;
        e->val = NULL;
        h->h.size -= 1;
        return 1;
    }
    return 0;
}

/* List helpers */

Value* list_from_array(Value** items, int n) {
    Value* lst = make_nil();
    for (int i = n - 1; i >= 0; --i) {
        lst = cons(items[i], lst);
    }
    return lst;
}

int list_length(Value* lst) {
    int n = 0;
    while (lst && lst->type == VAL_PAIR) {
        ++n;
        lst = lst->pair.cdr;
    }
    return n;
}

/* Truth and printing */

bool is_truthy(Value* v) {
    if (!v) {
        return false;
    }
    if (v->type == VAL_NIL) {
        return false;
    }
    if (v->type == VAL_BOOL && v->b == false) {
        return false;
    }
    return true;
}

static void print_list(Value* v);

static void print_string_escaped(const char* s, size_t len) {
    putchar('"');
    for (size_t i = 0; i < len; ++i) {
        unsigned char c = (unsigned char)s[i];
        switch (c) {
            case '\\': {
                fputs("\\\\", stdout);
            } break;
            case '\"': {
                fputs("\\\"", stdout);
            } break;
            case '\n': {
                fputs("\\n", stdout);
            } break;
            case '\r': {
                fputs("\\r", stdout);
            } break;
            case '\t': {
                fputs("\\t", stdout);
            } break;
            default: {
                if (c < 0x20) {
                    char buf[5];
                    snprintf(buf, sizeof(buf), "\\x%02X", c);
                    fputs(buf, stdout);
                } else {
                    putchar((char)c);
                }
            } break;
        }
    }
    putchar('"');
}

static void print_vector(Value* v) {
    printf("#(");
    for (size_t i = 0; i < v->vec.len; ++i) {
        print_value(v->vec.data[i] ? v->vec.data[i] : make_nil());
        if (i + 1 < v->vec.len) {
            printf(" ");
        }
    }
    printf(")");
}

static void print_hash(Value* v) {
    /* Simple descriptor to avoid huge dumps and cycles */
    printf("<hash %zu>", v->h.size);
}

void print_value(Value* v) {
    if (!v) {
        printf("nil");
        return;
    }
    switch (v->type) {
        case VAL_NIL: {
            printf("nil");
        } break;
        case VAL_INT: {
            printf("%ld", v->i);
        } break;
        case VAL_BOOL: {
            printf(v->b ? "#t" : "#f");
        } break;
        case VAL_SYMBOL: {
            printf("%s", v->sym);
        } break;
        case VAL_PAIR: {
            print_list(v);
        } break;
        case VAL_BUILTIN: {
            printf("<builtin>");
        } break;
        case VAL_CLOSURE: {
            printf("<closure>");
        } break;
        case VAL_STRING: {
            print_string_escaped(v->str.data ? v->str.data : "", v->str.len);
        } break;
        case VAL_VECTOR: {
            print_vector(v);
        } break;
        case VAL_HASH: {
            print_hash(v);
        } break;
    }
}

static void print_list(Value* v) {
    printf("(");
    while (v && v->type == VAL_PAIR) {
        print_value(v->pair.car);
        v = v->pair.cdr;
        if (v && v->type == VAL_PAIR) {
            printf(" ");
        }
    }
    if (v && v->type != VAL_NIL) {
        printf(" . ");
        print_value(v);
    }
    printf(")");
}

/* Symbol helper */

bool is_symbol(Value* v, const char* name) {
    return v && v->type == VAL_SYMBOL && v->sym && strcmp(v->sym, name) == 0;
}
