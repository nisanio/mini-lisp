#ifndef MINILISP_SYNTAX_H
#define MINILISP_SYNTAX_H

#include <stdint.h>
#include <stdbool.h>
#include "value.h"

/* --------------------------------------------------------------------------
 * Hygienic macro infrastructure — Stage 1 (scopes and syntax objects)
 *
 * This file introduces:
 *  - scope_id: unique scope markers
 *  - scope_set: an ordered, deduplicated set of scope ids
 *  - Syntax: a syntax object = (datum = Value*, scopes = scope_set)
 *
 * Nothing here evaluates or expands macros yet. It only provides the
 * data structures and utilities to attach and propagate scopes over ASTs.
 * -------------------------------------------------------------------------- */

/* Unique identifier for a scope mark */
typedef uint32_t scope_id;

/* A small ordered set of scope ids (ascending, no duplicates).
 * Implementation uses a simple dynamic array; suitable for a mini Lisp. */
typedef struct {
    scope_id* data;
    int       len;
    int       cap;
} scope_set;

/* A syntax object: a datum plus its scope marks */
typedef struct Syntax {
    Value*    datum;   /* underlying AST node (Value*) */
    scope_set scopes;  /* lexical marks attached to this syntax */
} Syntax;

/* ----- Scope set API ----------------------------------------------------- */

/* Initialize an empty scope set */
void ss_init(scope_set* s);

/* Free owned memory (does not free the Syntax or Value datum) */
void ss_free(scope_set* s);

/* Return true if the set contains 'id' */
bool ss_has(const scope_set* s, scope_id id);

/* Insert 'id' in ascending order if not present */
void ss_add(scope_set* s, scope_id id);

/* dst = a ∪ b (dst may alias a or b) */
void ss_union(scope_set* dst, const scope_set* a, const scope_set* b);

/* ----- Scope generation -------------------------------------------------- */

/* Return a fresh, globally unique scope id */
scope_id fresh_scope(void);

/* ----- Syntax objects ---------------------------------------------------- */

/* Allocate a new syntax object for given datum (scopes initially empty) */
Syntax* syn_new(Value* datum);

/* Shallow clone with one scope added (shares datum pointer) */
Syntax* syn_add_scope(const Syntax* x, scope_id id);

/* Map add-scope over the entire tree structure.
 * Returns a new Syntax whose datum is a deep-rebuilt list structure
 * (pairs reconstructed) that shares leaf atoms where safe. */
Syntax* syn_map_add_scope(const Syntax* x, scope_id id);

/* Deep copy of a Syntax (shares underlying atoms where appropriate).
 * The scope_set is copied by value. */
Syntax* syn_clone(const Syntax* x);

/* Free a Syntax object (does not free the underlying datum).
 * Ownership policy: Syntax only manages its own scope_set; Value* lifetime
 * is managed by the runtime/GC strategy of the interpreter (currently mallocs). */
void syn_free(Syntax* x);

#endif /* MINILISP_SYNTAX_H */
