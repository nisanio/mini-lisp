; Returns #t if the cons-list xs has a cycle, #f otherwise.
; Uses Floyd's tortoise–hare with safe cdr steps.

(defun has-cycle?
  (lambda (xs)
    (let* ((step  (lambda (p) (if (null? p) nil (cdr p))))
           (step2 (lambda (p) (step (step p))))
           (go    (lambda (slow fast)
                    (if (null? fast)
                        #f
                        (if (null? (step fast))
                            #f
                            (if (eq? slow fast)
                                #t
                                (go (step slow) (step2 fast))))))))
      (if (null? xs) #f (go xs (step xs))))))

; quick sanity checks (we cannot easily build a cyclic list without set-cdr!)
(print (has-cycle? '(1 2 3)))  ; => #f
(print (has-cycle? nil))       ; => #f
