#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "value.h"
#include "env.h"
#include "eval.h"
#include "reader.h"
#include "repl.h"
#include "builtins.h"

int main(int argc, char **argv) {
    /* Create global environment with builtins already installed. */
    Env *root = make_global();
    if (!root) {
        fprintf(stderr, "fatal: cannot create global environment\n");
        return 1;
    }

    if (argc > 1) {
        int exit_code = 0;
        for (int i = 1; i < argc; ++i) {
            exit_code |= run_file(root, argv[i]);
        }
        return exit_code ? 1 : 0;
    } else {
        run_repl(root);
        return 0;
    }
}
