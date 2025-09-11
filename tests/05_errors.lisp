;
; Stage 4 validation — error paths (invalid syntax-rules spec)
;
; Expected: error message for invalid define-syntax (and result 'nil')
;

; Missing (literals ...) list -> should error
(define-syntax broken
  (syntax-rules
    ((broken x) x)))

; Try to use it (should be no-op or error)
(broken 10)
