; Fibonacci-style DP: ways(n) = ways(n-1) + ways(n-2)
(defun climb-stairs
  (lambda (n)
    (if (<= n 2)
        n
        (let* ((a 1) (b 2)
               (go (lambda (i)
                     (if (< i n)
                         (begin
                           (let* ((c (+ a b)))
                             (set! a b)
                             (set! b c))
                           (go (+ i 1)))
                         b))))
          (go 2)))))

; quick test
(print (climb-stairs 5))  ; => 8
