; Merge two ascending sorted lists into one sorted list
(defun merge-sorted
  (lambda (xs ys)
    (if (null? xs)
        ys
        (if (null? ys)
            xs
            (let* ((a (car xs)) (b (car ys)))
              (if (<= a b)
                  (cons a (merge-sorted (cdr xs) ys))
                  (cons b (merge-sorted xs (cdr ys)))))))))

; quick test
(print (merge-sorted '(1 3 5) '(2 4 6)))  ; => (1 2 3 4 5 6)
