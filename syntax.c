#include "syntax.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* --------------------------------------------------------------------------
 * scope_set implementation
 * -------------------------------------------------------------------------- */

static void* xmalloc(size_t n) {
    void* p = malloc(n);
    if (!p) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    return p;
}

void ss_init(scope_set* s) {
    s->data = NULL;
    s->len  = 0;
    s->cap  = 0;
}

void ss_free(scope_set* s) {
    if (s && s->data) {
        free(s->data);
        s->data = NULL;
    }
    s->len = s->cap = 0;
}

static void ss_reserve(scope_set* s, int want) {
    if (s->cap >= want) return;
    int cap = s->cap ? s->cap * 2 : 4;
    if (cap < want) cap = want;
    s->data = (scope_id*)realloc(s->data, (size_t)cap * sizeof(scope_id));
    if (!s->data) {
        fprintf(stderr, "OOM\n");
        exit(1);
    }
    s->cap = cap;
}

bool ss_has(const scope_set* s, scope_id id) {
    /* binary search (ascending order) */
    int lo = 0, hi = s->len;
    while (lo < hi) {
        int mid = lo + (hi - lo) / 2;
        scope_id v = s->data[mid];
        if (v == id) return true;
        if (v < id)  lo = mid + 1;
        else         hi = mid;
    }
    return false;
}

void ss_add(scope_set* s, scope_id id) {
    /* insert in ascending order if missing */
    int lo = 0, hi = s->len;
    while (lo < hi) {
        int mid = lo + (hi - lo) / 2;
        scope_id v = s->data[mid];
        if (v == id) return; /* already present */
        if (v < id)  lo = mid + 1;
        else         hi = mid;
    }
    ss_reserve(s, s->len + 1);
    /* shift right from lo */
    memmove(&s->data[lo + 1], &s->data[lo], (size_t)(s->len - lo) * sizeof(scope_id));
    s->data[lo] = id;
    s->len += 1;
}

void ss_union(scope_set* dst, const scope_set* a, const scope_set* b) {
    /* classic merge of sorted unique arrays */
    int i = 0, j = 0;
    ss_init(dst);
    if (!a && !b) return;
    int cap = (a ? a->len : 0) + (b ? b->len : 0);
    if (cap < 4) cap = 4;
    dst->data = (scope_id*)xmalloc((size_t)cap * sizeof(scope_id));
    dst->cap  = cap;
    dst->len  = 0;

    while ((a && i < a->len) || (b && j < b->len)) {
        scope_id va = (a && i < a->len) ? a->data[i] : UINT32_MAX;
        scope_id vb = (b && j < b->len) ? b->data[j] : UINT32_MAX;
        scope_id pick = (va < vb) ? va : vb;
        if (va < vb) i++; else if (vb < va) j++; else { i++; j++; }
        /* append pick if new */
        if (dst->len == 0 || dst->data[dst->len - 1] != pick) {
            dst->data[dst->len++] = pick;
        }
    }
}

/* --------------------------------------------------------------------------
 * fresh scope ids
 * -------------------------------------------------------------------------- */

scope_id fresh_scope(void) {
    static uint32_t counter = 1; /* 0 is reserved to mean "no-scope" */
    return counter++;
}

/* --------------------------------------------------------------------------
 * Syntax object
 * -------------------------------------------------------------------------- */

Syntax* syn_new(Value* datum) {
    Syntax* x = (Syntax*)xmalloc(sizeof(Syntax));
    x->datum = datum;
    ss_init(&x->scopes);
    return x;
}

Syntax* syn_clone(const Syntax* x) {
    if (!x) return NULL;
    Syntax* y = (Syntax*)xmalloc(sizeof(Syntax));
    y->datum = x->datum; /* shallow datum copy */
    ss_init(&y->scopes);
    /* copy scopes */
    if (x->scopes.len) {
        y->scopes.data = (scope_id*)xmalloc((size_t)x->scopes.len * sizeof(scope_id));
        memcpy(y->scopes.data, x->scopes.data, (size_t)x->scopes.len * sizeof(scope_id));
        y->scopes.len = y->scopes.cap = x->scopes.len;
    }
    return y;
}

Syntax* syn_add_scope(const Syntax* x, scope_id id) {
    if (!x) return NULL;
    Syntax* y = syn_clone(x);
    ss_add(&y->scopes, id);
    return y;
}

static Value* rebuild_with_added_scope(Value* v, scope_id id);

/* Helper: add-scope to a single atom node.
 * Atoms share underlying allocation; only syntax scopes carry hygiene marks. */
static Value* add_scope_atom(Value* v, scope_id id) {
    (void)id; /* atom data remains as is; scope lives in Syntax wrapper */
    return v;
}

/* Rebuild a list (PAIR/NIL) and add scope to each element's syntax wrapper.
 * We do not allocate Syntax here; this function returns a Value* tree. */
static Value* rebuild_list_with_added_scope(Value* v, scope_id id) {
    if (!v || v->type == VAL_NIL) return v;
    if (v->type != VAL_PAIR) return v;

    /* Rebuild spine */
    Value* new_car = rebuild_with_added_scope(car(v), id);
    Value* new_cdr = rebuild_list_with_added_scope(cdr(v), id);
    return cons(new_car, new_cdr);
}

static Value* rebuild_with_added_scope(Value* v, scope_id id) {
    if (!v) return v;
    switch (v->type) {
        case VAL_NIL:
        case VAL_INT:
        case VAL_BOOL:
        case VAL_SYMBOL:
        case VAL_BUILTIN:
        case VAL_CLOSURE:
            return add_scope_atom(v, id);
        case VAL_PAIR:
            return rebuild_list_with_added_scope(v, id);
        default:
            return v;
    }
}

/* Public mapping: returns a Syntax* with datum rebuilt and scope added.
 * The returned Syntax carries (x->scopes ∪ {id}). */
Syntax* syn_map_add_scope(const Syntax* x, scope_id id) {
    if (!x) return NULL;
    Syntax* y = syn_clone(x);
    /* Rebuild the datum tree (pairs) so that future passes can compare by pointer
     * if needed while keeping atoms shared. */
    y->datum = rebuild_with_added_scope(x->datum, id);
    ss_add(&y->scopes, id);
    return y;
}

void syn_free(Syntax* x) {
    if (!x) return;
    ss_free(&x->scopes);
    free(x);
}
