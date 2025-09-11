; Return the max sum over all contiguous subarrays
(defun max-subarray
  (lambda (v)
    (let* ((n (vector-length v)))
      (if (= n 0)
          0
          (let* ((best (vector-ref v 0))
                 (cur  (vector-ref v 0))
                 (go (lambda (i)
                       (if (< i n)
                           (begin
                             (let* ((x (vector-ref v i)))
                               (set! cur (if (> (+ cur x) x) (+ cur x) x))
                               (if (> cur best) (set! best cur) nil))
                             (go (+ i 1)))
                           best))))
            (go 1))))))

; quick test
(print (max-subarray (vector -2 1 -3 4 -1 2 1 -5 4))) ; => 6
