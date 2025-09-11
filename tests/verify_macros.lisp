;; tests/verify_macros.lisp
;;
;; Este archivo termina en silencio si todo está OK.
;; Si algo falla, verás un error "unbound symbol: TEST-FAIL-..."
;; indicando exactamente qué caso se rompió.

(define-syntax when
  (syntax-rules ()
    ((when t x) (if t x nil))))

;; 1) when con #t debe devolver 42
;; Si NO es 42, forzamos un símbolo no ligado para ver un mensaje claro.
(if (= (when #t 42) 42)
    0
    TEST-FAIL-WHEN-TRUE)

;; 2) when con #f debe devolver nil (falso)
;; Si NO es nil/falso, forzamos un símbolo no ligado con nombre claro.
(if (when #f 42)
    TEST-FAIL-WHEN-FALSE
    0)

;; Si llegaste hasta aquí, ambas pruebas pasaron y no se imprime nada.
