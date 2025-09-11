;
; Stage 4 validation — AND macro with ellipsis and recursion
;
; Expected output:
;   #t
;   #f
;   #f
;   7
;

(define-syntax my-and
  (syntax-rules ()
    ((my-and) #t)
    ((my-and x) x)
    ((my-and x y ...) (if x (my-and y ...) #f))))

(my-and)                  ; => #t
(my-and #t #t #f #t)      ; => #f
(my-and #f 123)           ; => #f
(my-and #t 7)             ; => 7
