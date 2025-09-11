;
; Stage 4 validation — UNLESS macro
;
; Expected output:
;   nil
;   99
;

(define-syntax unless
  (syntax-rules ()
    ((unless test body)
     (if test nil body))))

(unless #t  99) ; => nil
(unless #f  99) ; => 99
