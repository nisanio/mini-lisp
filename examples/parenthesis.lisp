; Validate (), {}, [] using a simple stack
(defun valid-parens
  (lambda (s)
    (let* ((n (string-length s))
           (pairs (lambda (c)
                    (if (= c 41) 40          ; ) -> (
                        (if (= c 93) 91      ; ] -> [
                            (if (= c 125) 123 ; } -> {
                                -1)))))
           (is-open? (lambda (c) (or (= c 40) (= c 91) (= c 123))))
           (stack-push (lambda (st c) (cons c st)))
           (stack-pop  (lambda (st) (if (null? st) (list -1 nil) (list (car st) (cdr st))))))
      (let* ((go (lambda (i st)
                   (if (>= i n)
                       (null? st)
                       (let* ((c (string-ref s i)))
                         (if (is-open? c)
                             (go (+ i 1) (stack-push st c))
                             (let* ((need (pairs c))
                                    (res (stack-pop st))
                                    (top (car res))
                                    (rest (car (cdr res))))
                               (if (or (= need -1) (not (= need top)))
                                   #f
                                   (go (+ i 1) rest)))))))))
        (go 0 nil)))))

; quick tests
(print (valid-parens "()[]{}"))      ; => #t
(print (valid-parens "(]"))          ; => #f
(print (valid-parens "([{}])"))      ; => #t
