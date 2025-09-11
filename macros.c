// macros.c — Mini-Lisp
// Notes (English):
// - Fix 1: Free identifiers in templates are emitted as datum symbols (not NIL).
// - Fix 2: Pattern matching enforces that the first element of a rule pattern
//          is the macro keyword (literal head match).
// - Fix 3: Ellipses reification splices matches INSIDE the current list
//          (no flattening at the parent level). Needed for (and …).

#include "macros.h"
#include "syntax.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Portable strdup replacement (avoids missing prototype under strict C standards) */
static char* xstrdup(const char* s) {
    if (!s) return NULL;
    size_t n = strlen(s) + 1;
    char* p = (char*)malloc(n);
    if (!p) { fprintf(stderr, "OOM\n"); exit(1); }
    memcpy(p, s, n);
    return p;
}

#include <stdio.h>
#include <stdbool.h>

/* --------------------------------------------------------------------------
 * Small utilities
 * -------------------------------------------------------------------------- */

static void* xmalloc(size_t n) {
    void* p = malloc(n);
    if (!p) { fprintf(stderr, "OOM\n"); exit(1); }
    return p;
}

static bool is_list(const Value* v) {
    return v && (v->type == VAL_PAIR || v->type == VAL_NIL);
}

bool is_symbol_named(const Value* v, const char* name) {
    return v && v->type == VAL_SYMBOL && v->sym && strcmp(v->sym, name) == 0;
}

/* Provided by value.h; const wrapper here */
extern int list_length(Value* lst);
static int list_length_c(const Value* v) { return list_length((Value*)v); }

/* List <-> array helpers (shallow) */
static int list_to_array(const Value* lst, Value*** out) {
    int n = list_length_c(lst);
    if (n < 0) return -1;
    Value** arr = (Value**)xmalloc((size_t)n * sizeof(Value*));
    int i = 0;
    const Value* it = lst;
    while (it && it->type == VAL_PAIR) {
        arr[i++] = car((Value*)it);
        it = cdr((Value*)it);
    }
    *out = arr;
    return n;
}

static Value* array_to_list(Value** arr, int n) {
    Value* out = make_nil();
    for (int i = n - 1; i >= 0; --i) out = cons(arr[i], out);
    return out;
}

/* --------------------------------------------------------------------------
 * syntax-rules compiler
 * -------------------------------------------------------------------------- */

static bool parse_syntax_rules(const Syntax* spec,
                               Value*** out_literals, int* out_litc,
                               SRRule** out_rules,   int* out_rulec) {
    // spec = (syntax-rules (lits...) (pat tmpl) (pat tmpl) ...)
    const Value* d = spec->datum;
    if (!is_list(d) || d->type != VAL_PAIR) return false;

    const Value* head = car((Value*)d);
    if (!is_symbol_named(head, "syntax-rules")) return false;

    const Value* tail = cdr((Value*)d);
    if (!(tail && tail->type == VAL_PAIR)) return false;

    // literals
    const Value* lits_list = car((Value*)tail);
    if (!is_list(lits_list)) return false;

    Value** lits_arr = NULL;
    int litc = list_to_array(lits_list, &lits_arr);
    if (litc < 0) { free(lits_arr); return false; }
    for (int i = 0; i < litc; ++i) {
        if (!(lits_arr[i] && lits_arr[i]->type == VAL_SYMBOL)) {
            free(lits_arr); return false;
        }
    }

    // rules: (pat tmpl)
    const Value* rules_tail = cdr((Value*)tail);
    int rulec = list_length_c(rules_tail);
    if (rulec < 0) { free(lits_arr); return false; }

    SRRule* rules = (SRRule*)xmalloc((size_t)rulec * sizeof(SRRule));
    int idx = 0;
    const Value* it = rules_tail;
    while (it && it->type == VAL_PAIR) {
        const Value* rule = car((Value*)it);
        if (!(rule && rule->type == VAL_PAIR)) { free(rules); free(lits_arr); return false; }
        const Value* r_pat  = car((Value*)rule);
        const Value* r_tail = cdr((Value*)rule);
        if (!(r_tail && r_tail->type == VAL_PAIR &&
              cdr((Value*)r_tail) && cdr((Value*)r_tail)->type == VAL_NIL)) {
            free(rules); free(lits_arr); return false;
        }
        const Value* r_tmpl = car((Value*)r_tail);

        rules[idx].pattern    = (Value*)r_pat;
        rules[idx].template_  = (Value*)r_tmpl;
        idx++;
        it = cdr((Value*)it);
    }

    *out_literals = lits_arr;
    *out_litc     = litc;
    *out_rules    = rules;
    *out_rulec    = rulec;
    return true;
}

SyntaxTransformer* sr_compile(const Syntax* spec) {
    if (!spec) return NULL;

    Value** lits = NULL; int litc = 0;
    SRRule* rules = NULL; int rulec = 0;

    if (!parse_syntax_rules(spec, &lits, &litc, &rules, &rulec)) return NULL;

    SyntaxTransformer* tr = (SyntaxTransformer*)xmalloc(sizeof(SyntaxTransformer));
    tr->ruleset = (SRRuleset*)xmalloc(sizeof(SRRuleset));
    tr->ruleset->literals   = lits;
    tr->ruleset->lit_count  = litc;
    tr->ruleset->rules      = rules;
    tr->ruleset->rule_count = rulec;
    return tr;
}

void sr_free(SyntaxTransformer* tr) {
    if (!tr) return;
    if (tr->ruleset) {
        if (tr->ruleset->literals) free(tr->ruleset->literals);
        if (tr->ruleset->rules)    free(tr->ruleset->rules);
        free(tr->ruleset);
    }
    free(tr);
}

/* --------------------------------------------------------------------------
 * Pattern matching (single-level ellipsis)
 * -------------------------------------------------------------------------- */

typedef struct {
    const Value* var_sym;
    Value**      matches;
    int          count;
    int          cap;
} Binding;

typedef struct {
    Binding* arr;
    int      len;
    int      cap;
} Bindings;

static void b_init(Bindings* b) { b->arr = NULL; b->len = 0; b->cap = 0; }

static void b_free(Bindings* b) {
    if (!b) return;
    if (b->arr) {
        for (int i = 0; i < b->len; ++i) free(b->arr[i].matches);
        free(b->arr);
    }
    b->arr = NULL; b->len = 0; b->cap = 0;
}

static void b_reserve(Bindings* b, int want) {
    if (b->cap >= want) return;
    int cap = b->cap ? (b->cap * 2) : 8;
    if (cap < want) cap = want;
    b->arr = (Binding*)realloc(b->arr, (size_t)cap * sizeof(Binding));
    if (!b->arr) { fprintf(stderr, "OOM\n"); exit(1); }
    b->cap = cap;
}

static Binding* b_find(Bindings* b, const Value* sym) {
    for (int i = 0; i < b->len; ++i) {
        const Value* a = b->arr[i].var_sym;
        if (a == sym) return &b->arr[i];
        if (a && sym &&
            a->type == VAL_SYMBOL && sym->type == VAL_SYMBOL &&
            strcmp(a->sym, sym->sym) == 0)
            return &b->arr[i];
    }
    return NULL;
}

static Binding* b_add(Bindings* b, const Value* sym) {
    b_reserve(b, b->len + 1);
    Binding* e = &b->arr[b->len++];
    e->var_sym = sym;
    e->matches = NULL;
    e->count   = 0;
    e->cap     = 0;
    return e;
}

static void e_push(Binding* e, Value* v) {
    int want = e->count + 1;
    if (e->cap < want) {
        int cap = e->cap ? (e->cap * 2) : 4;
        if (cap < want) cap = want;
        e->matches = (Value**)realloc(e->matches, (size_t)cap * sizeof(Value*));
        if (!e->matches) { fprintf(stderr, "OOM\n"); exit(1); }
        e->cap = cap;
    }
    e->matches[e->count++] = v;
}

static bool is_literal_symbol(const SRRuleset* rs, const Value* s) {
    if (!(s && s->type == VAL_SYMBOL)) return false;
    for (int i = 0; i < rs->lit_count; ++i) {
        if (strcmp(rs->literals[i]->sym, s->sym) == 0) return true;
    }
    return false;
}

static bool pat_is_ellipsis(const Value* v) { return is_symbol_named(v, "..."); }
static bool pat_is_wildcard(const Value* v) { return is_symbol_named(v, "_"); }

static bool match_pattern(const SRRuleset* rs, const Value* pat, const Value* form, Bindings* out);

/* Enforce literal head match for macro keyword, then match the rest (supports '...'). */
static bool match_list(const SRRuleset* rs, const Value* pat, const Value* form, Bindings* out) {
    Value** p_arr = NULL; int pn = list_to_array(pat,  &p_arr);
    if (pn < 0) { free(p_arr); return false; }
    Value** f_arr = NULL; int fn = list_to_array(form, &f_arr);
    if (fn < 0) { free(p_arr); free(f_arr); return false; }

    int pi = 0, fi = 0;

    if (pn > 0 && p_arr[0] && p_arr[0]->type == VAL_SYMBOL) {
        if (!(fn > 0 && f_arr[0] && f_arr[0]->type == VAL_SYMBOL &&
              strcmp(p_arr[0]->sym, f_arr[0]->sym) == 0)) {
            free(p_arr); free(f_arr); return false;
        }
        pi = 1; fi = 1;
    }

    while (pi < pn) {
        if (pi + 1 < pn && pat_is_ellipsis(p_arr[pi + 1])) {
            const Value* unit = p_arr[pi];
            while (fi < fn) {
                if (unit && unit->type == VAL_SYMBOL &&
                    !is_literal_symbol(rs, unit) && !pat_is_wildcard(unit)) {
                    Binding* b = b_find(out, unit);
                    if (!b) b = b_add(out, unit);
                    e_push(b, f_arr[fi]);
                    fi++;
                } else {
                    if (!match_pattern(rs, unit, f_arr[fi], out)) break;
                    fi++;
                }
            }
            pi += 2;
            continue;
        }
        if (fi >= fn) { free(p_arr); free(f_arr); return false; }
        if (!match_pattern(rs, p_arr[pi], f_arr[fi], out)) {
            free(p_arr); free(f_arr); return false;
        }
        pi++; fi++;
    }

    bool ok = (fi == fn);
    free(p_arr); free(f_arr);
    return ok;
}

static bool match_pattern(const SRRuleset* rs, const Value* pat, const Value* form, Bindings* out) {
    if (pat_is_wildcard(pat)) return true;

    if (pat && pat->type == VAL_SYMBOL && is_literal_symbol(rs, pat)) {
        return (form && form->type == VAL_SYMBOL &&
                strcmp(form->sym, pat->sym) == 0);
    }

    if (pat && pat->type == VAL_SYMBOL) {
        Binding* b = b_find(out, pat);
        if (!b) b = b_add(out, pat);
        e_push(b, (Value*)form);
        return true;
    }

    if (pat && pat->type == VAL_PAIR) {
        if (!(form && (form->type == VAL_PAIR || form->type == VAL_NIL))) return false;
        return match_list(rs, pat, form, out);
    }

    if (!pat && !form) return true;
    if (!pat || !form) return false;
    if (pat->type != form->type) return false;

    switch (pat->type) {
        case VAL_INT:    return pat->i == form->i;
        case VAL_BOOL:   return pat->b == form->b;
        case VAL_SYMBOL: return strcmp(pat->sym, form->sym) == 0;
        default:         return pat == form;
    }
}

/* --------------------------------------------------------------------------
 * Template reification (single-level ellipsis)
 * -------------------------------------------------------------------------- */

static Value* reify_node(const SRRuleset* rs, const Value* node, const Bindings* b); // fwd

/* Build list from all matches of var_sym (preserving order). */
static Value* list_from_matches(const Bindings* b, const Value* var_sym) {
    for (int i = 0; i < b->len; ++i) {
        const Binding* e = &b->arr[i];
        if (e->var_sym && var_sym &&
            e->var_sym->type == VAL_SYMBOL && var_sym->type == VAL_SYMBOL &&
            strcmp(e->var_sym->sym, var_sym->sym) == 0) {
            return array_to_list(e->matches, e->count);
        }
    }
    return make_nil();
}

/* Reify a template list WITHOUT flattening at parent level.
   (var ...) splices inside this list only. */
static Value* reify_list(const SRRuleset* rs, const Value* tmpl, const Bindings* b) {
    Value** arr = NULL; int n = list_to_array(tmpl, &arr);
    if (n < 0) { free(arr); return (Value*)tmpl; }

    Value* out = make_nil(); // build right -> left
    for (int i = n - 1; i >= 0; --i) {
        if (i - 1 >= 0 && is_symbol_named(arr[i], "...") && arr[i - 1]->type == VAL_SYMBOL) {
            Value* seq = list_from_matches(b, arr[i - 1]);
            const Value* it = seq;
            while (it && it->type == VAL_PAIR) {
                out = cons(car((Value*)it), out);
                it = cdr((Value*)it);
            }
            i -= 1; // skip the var preceding "..."
            continue;
        }
        out = cons(reify_node(rs, arr[i], b), out);
    }

    free(arr);
    return out;
}

/* First match (or NULL) for a pattern variable. */
static Value* binding_first(const Bindings* b, const Value* var_sym) {
    for (int i = 0; i < b->len; ++i) {
        const Binding* e = &b->arr[i];
        if (e->var_sym && var_sym &&
            e->var_sym->type == VAL_SYMBOL && var_sym->type == VAL_SYMBOL &&
            strcmp(e->var_sym->sym, var_sym->sym) == 0) {
            return (e->count > 0) ? e->matches[0] : NULL;
        }
    }
    return NULL;
}

static Value* reify_node(const SRRuleset* rs, const Value* node, const Bindings* b) {
    (void)rs;
    if (!node) return (Value*)node;

    if (node->type == VAL_SYMBOL) {
        Value* bound = binding_first(b, node);
        if (bound) return bound;    // pattern variable
        return (Value*)node;        // free identifier: keep datum symbol as-is
    }
    if (node->type == VAL_PAIR) {
        return reify_list(rs, node, b);
    }
    return (Value*)node;
}

/* --------------------------------------------------------------------------
 * Public expansion API
 * -------------------------------------------------------------------------- */

Syntax* sr_expand_call(struct Env* env, const SyntaxTransformer* tr, const Syntax* call) {
    (void)env;
    if (!tr || !tr->ruleset || !call) return NULL;

    const SRRuleset* rs = tr->ruleset;
    const Value* d = call->datum;
    if (!(d && d->type == VAL_PAIR)) return NULL;

    for (int i = 0; i < rs->rule_count; ++i) {
        const Value* pat  = rs->rules[i].pattern;
        const Value* tmpl = rs->rules[i].template_;
        Bindings b; b_init(&b);
        bool ok = match_pattern(rs, pat, d, &b);
        if (ok) {
            Value* out_datum = reify_node(rs, tmpl, &b);
            b_free(&b);
            return syn_new(out_datum);
        }
        b_free(&b);
    }
    return NULL;
}

/* --------------------------------------------------------------------------
 * Macro registry
 * -------------------------------------------------------------------------- */

typedef struct MacroEntry {
    char* name;
    SyntaxTransformer* tr;
    struct MacroEntry* next;
} MacroEntry;

static MacroEntry* G_macros = NULL;

static MacroEntry* macro_find(const char* name) {
    for (MacroEntry* it = G_macros; it; it = it->next)
        if (strcmp(it->name, name) == 0) return it;
    return NULL;
}

void macro_registry_clear(void) {
    MacroEntry* it = G_macros;
    while (it) {
        MacroEntry* nx = it->next;
        if (it->tr) sr_free(it->tr);
        free(it->name);
        free(it);
        it = nx;
    }
    G_macros = NULL;
}

int macro_define(const char* name, SyntaxTransformer* tr) {
    if (!name || !tr) return 0;
    MacroEntry* e = macro_find(name);
    if (e) {
        if (e->tr) sr_free(e->tr);
        e->tr = tr;
        return 1;
    }
    e = (MacroEntry*)xmalloc(sizeof(MacroEntry));
    e->name = xstrdup(name);
    e->tr   = tr;
    e->next = G_macros;
    G_macros = e;
    return 1;
}

const SyntaxTransformer* macro_lookup(const char* name) {
    MacroEntry* e = macro_find(name);
    return e ? e->tr : NULL;
}

/* --------------------------------------------------------------------------
 * Hygienic single-step expansion
 * -------------------------------------------------------------------------- */

Syntax* macroexpand1_hygienic(struct Env* env, const Syntax* form) {
    if (!form || !form->datum || form->datum->type != VAL_PAIR) return NULL;

    Value* head = car(form->datum);
    if (!(head && head->type == VAL_SYMBOL)) return NULL;

    const SyntaxTransformer* tr = macro_lookup(head->sym);
    if (!tr) return NULL;

    scope_id S_use = fresh_scope();
    Syntax* marked_call = syn_map_add_scope(form, S_use);

    Syntax* raw = sr_expand_call(env, tr, marked_call);
    if (!raw) { syn_free(marked_call); return NULL; }

    scope_id S_intro = fresh_scope();
    Syntax* hyg = syn_map_add_scope(raw, S_intro);

    syn_free(marked_call);
    syn_free(raw);
    return hyg;
}