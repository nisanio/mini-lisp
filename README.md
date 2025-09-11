Mini Lisp in C — Features 
-----------------------------------

Types:
- nil
- int
- bool
- symbol
- pair (list)
- builtin
- closure

Special forms:
- quote
- if
- df (define)
- set!
- lambda
- begin
- and, or

Booleans:
- #t, #f
- Truth: only #f and nil are false

Builtins (arithmetic):
- + - * / % abs max min

Builtins (comparison):
- = < > <= >= eq? equal? not

Builtins (lists):
- cons car cdr list null? length

Builtins (io):
- print

Modes:
- Run a file:   ./minilisp program.lisp
- Interactive:  ./minilisp

Phase 4 highlights:
- List toolkit: cons/car/cdr/list/null?/length
- Deep structural equality: equal?
- Extra numerics: %, abs, max, min
- Short-circuit special forms: and, or
- print: prints a value (with newline) and returns it


```
; ============================================================================
; Quick demo for Phase 4 lists & comparisons
; Run: ./minilisp examples_lists.lisp

; ----- Lists -----
(df xs (list 1 2 3))
(print xs)                      ; (1 2 3)
(print (length xs))             ; 3
(print (car xs))                ; 1
(print (cdr xs))                ; (2 3)
(print (null? xs))              ; #f
(print (null? nil))             ; #t
(print (equal? xs '(1 2 3)))    ; #t
(print (equal? xs '(1 2 (3))))  ; #f

; cons and structural equality
(df ys (cons 0 xs))
(print ys)                      ; (0 1 2 3)
(print (equal? ys '(0 1 2 3)))  ; #t

; ----- Numerics -----
(print (% 10 3))                ; 1
(print (abs -42))               ; 42
(print (max 1 9 3 7))           ; 9
(print (min 1 9 3 7))           ; 1
(print (= 3 3 3))               ; #t
(print (< 1 2 3))               ; #t
(print (>= 3 3 3))              ; #t

; ----- Short-circuit and/or -----
(print (and #t 1 2 3))          ; 3 (last truthy)
(print (and #t #f 9))           ; #f (first falsy)
(print (or #f nil 0 7))         ; 0 (first truthy; note 0 is truthy)
(print (or #f nil #f))          ; #f (all falsy → last)

; ----- print returns its argument -----
(print (print 99))              ; prints "99" then "99" again

; End
```