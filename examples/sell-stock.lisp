; Input: vector of prices; Output: maximum profit (0 if none)
(defun max-profit
  (lambda (v)
    (let* ((n (vector-length v)))
      (if (<= n 1)
          0
          (let* ((minp (vector-ref v 0))
                 (best 0)
                 (go (lambda (i)
                       (if (< i n)
                           (begin
                             (let* ((p (vector-ref v i))
                                    (profit (- p minp)))
                               (if (> profit best) (set! best profit) nil)
                               (if (< p minp)      (set! minp p)      nil))
                             (go (+ i 1)))
                           best))))
            (go 1))))))

; quick test
(print (max-profit (vector 7 1 5 3 6 4)))  ; => 5
