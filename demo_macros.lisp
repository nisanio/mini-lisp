;; Demo: macros básicas con syntax-rules

(define-syntax when
  (syntax-rules ()
    ((when test body ...) (if test (begin body ...) nil))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...) (if test nil (begin body ...)))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or x) x)
    ((or x y ...) (if x x (or y ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and x) x)
    ((and x y ...) (if x (and y ...) #f))))

(print (when #t  42))          ; => 42
(print (when #f  42))          ; => nil
(print (unless #t 42))         ; => nil
(print (unless #f 42))         ; => 42
(print (or #f #f 7))           ; => 7
(print (and #t #t 7))          ; => 7
