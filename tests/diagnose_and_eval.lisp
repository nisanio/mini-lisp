;; tests/diagnose_and_eval.lisp
;;
;; Verifica la EVALUACIÓN de 'and' sin usar '='.

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and x) x)
    ((and x y ...) (if x (and y ...) #f))))

;; (and) debe ser verdadero
(if (and) 0 TEST-FAIL-AND-EMPTY-EVAL)

;; (and #t 42) debe ser verdadero (no nil)
(if (and #t 42) 0 TEST-FAIL-AND-TRUE-TAIL-EVAL)

;; (and #f 42) debe ser falso
(if (and #f 42) TEST-FAIL-AND-FALSE-HEAD-EVAL 0)

;; (and #t #t #t) debe ser verdadero
(if (and #t #t #t) 0 TEST-FAIL-AND-ALL-TRUE-EVAL)

;; (and #t #f #t) debe ser falso
(if (and #t #f #t) TEST-FAIL-AND-MIDDLE-FALSE-EVAL 0)
