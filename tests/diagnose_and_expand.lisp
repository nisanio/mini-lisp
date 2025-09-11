;; tests/diagnose_and_expand.lisp
;;
;; Este archivo falla (con nombre claro) si la expansión de 'and' no es
;; la esperada para cada caso. No usa '=', ni pega en REPL.

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and x) x)
    ((and x y ...) (if x (and y ...) #f))))

;; 1) (and)  ==> #t   (no debería expandir a una lista)
(define EXP1 (macroexpand '(and)))
(if (pair? EXP1)
    TEST-FAIL-EXPANSION-AND-EMPTY-IS-LIST
    0)

;; 2) (and #t 42) ==> (if #t (and 42) #f)
(define EXP2 (macroexpand '(and #t 42)))
;; La cabeza debe ser 'if:
(if (pair? EXP2) 0 TEST-FAIL-EXP2-NOT-PAIR)
(if (eq? (car EXP2) 'if) 0 TEST-FAIL-EXP2-HEAD-NOT-IF)
;; El 'then' debe ser (and 42):
(define EXP2-THEN (car (cdr EXP2)))
(if (and (pair? EXP2-THEN)
         (eq? (car EXP2-THEN) 'and)
         (pair? (cdr EXP2-THEN))
         (eq? (car (
