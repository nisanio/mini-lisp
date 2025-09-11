;
; Stage 4 validation — dump of macroexpansions to check nested rewrites
;
; Expected output (shape, not exact object identity):
;   (if #t 1 nil)
;   (if (if #t 1 nil) 2 nil)
;

(define-syntax when
  (syntax-rules ()
    ((when test body)
     (if test body nil))))

(macroexpand '(when #t 1))
(macroexpand '(when (when #t 1) 2))
