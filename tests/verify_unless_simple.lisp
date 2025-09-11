;; Macro adicional para asegurar que el patrón con '...' y cabeza libre funciona.

(define-syntax unless
  (syntax-rules ()
    ((unless t x) (if t nil x))
    ((unless t x y ...) (if t nil (begin x y ...)))))  ;; si no tienes 'begin', puedes quitarlo

;; Básicos de unless
(if (unless #t 1)  TEST-FAIL-UNLESS-T-1 0)  ;; con #t debe ser falso
(if (unless #f 1)  0 TEST-FAIL-UNLESS-F-1)  ;; con #f debe ser verdadero
