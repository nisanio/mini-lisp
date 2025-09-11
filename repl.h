#ifndef MINILISP_REPL_H
#define MINILISP_REPL_H

#include "env.h"

/* Execute all forms contained in a file buffer. Returns 0 on success, non-zero on open error. */
int run_file(Env* global, const char* path);

/* Interactive REPL loop (prints result of each top-level form). */
void run_repl(Env* global);

#endif /* MINILISP_REPL_H */
