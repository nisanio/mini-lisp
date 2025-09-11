#ifndef MINILISP_MACROS_H
#define MINILISP_MACROS_H

#include <stdbool.h>
#include <stdint.h>
#include "value.h"
#include "syntax.h"

struct Env;

/* (pattern template) pair */
typedef struct {
    Value* pattern;
    Value* template_;
} SRRule;

typedef struct {
    Value** literals;
    int     lit_count;
    SRRule* rules;
    int     rule_count;
} SRRuleset;

typedef struct {
    SRRuleset* ruleset;
} SyntaxTransformer;

/* ---- compiler ---- */
SyntaxTransformer* sr_compile(const Syntax* spec_syntax);
void sr_free(SyntaxTransformer* tr);

/* Low-level: try to expand a specific call using a given transformer. */
Syntax* sr_expand_call(struct Env* env, const SyntaxTransformer* tr, const Syntax* call);

/* ---- global macro registry (by operator symbol name) ---- */
void macro_registry_clear(void);
int  macro_define(const char* name, SyntaxTransformer* tr); /* takes ownership of tr */
const SyntaxTransformer* macro_lookup(const char* name);

/* ---- hygienic single-step expansion using the registry ----
 * Returns a NEW Syntax* if expanded, or NULL if the form is not a macro call.
 * Applies call-site and introduced scopes.
 */
Syntax* macroexpand1_hygienic(struct Env* env, const Syntax* form);

/* Utility */
bool is_symbol_named(const Value* v, const char* name);

#endif /* MINILISP_MACROS_H */
