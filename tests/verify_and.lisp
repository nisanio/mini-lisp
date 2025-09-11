;; tests/verify_and.lisp
;;
;; Termina en silencio si TODO está OK.
;; Si algo falla, verás "unbound symbol: TEST-FAIL-...".

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and x) x)
    ((and x y ...) (if x (and y ...) #f))))

;; 1) (and) => #t
(if (and) 0 TEST-FAIL-AND-EMPTY)

;; 2) (and #t 42) => 42
(if (= (and #t 42) 42) 0 TEST-FAIL-AND-TRUE-TAIL)

;; 3) (and #f 42) => #f / nil
(if (and #f 42) TEST-FAIL-AND-FALSE-HEAD 0)

;; 4) (and #t #t #t) => #t
(if (and #t #t #t) 0 TEST-FAIL-AND-ALL-TRUE)

;; 5) (and #t #f #t) => #f
(if (and #t #f #t) TEST-FAIL-AND-MIDDLE-FALSE 0)
