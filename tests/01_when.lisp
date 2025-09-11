;
; Stage 4 validation — WHEN macro + automatic expansion
;
; Expected output (in order of forms):
;   (if #t 10 nil)
;   (if #f 20 nil)
;   42
;   nil
;

(define-syntax when
  (syntax-rules ()
    ((when test body)
     (if test body nil))))

; Manual inspection via macroexpanders
(macroexpand-1 '(when #t 10))
(macroexpand   '(when #f 20))

; Automatic expansion before eval
(when #t 42)
(when #f 42)
