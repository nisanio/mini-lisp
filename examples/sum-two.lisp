; ============================================================================
; program.lisp
; Reads numbers and a target from values.txt, finds all pairs that sum to target,
; and writes them into result.txt
; ============================================================================

; --- helper: split a comma-separated line into a list of integers ---
(defun parse-csv-line
  (lambda (s)
    (map (lambda (tok) (string->number tok))
         (split1 s 44))))   ; 44 = ASCII code for ','

; --- convert string to number (assuming valid integer) ---
(defun string->number
  (lambda (s)
    (let* ((n (string-length s))
           (sign (if (and (> n 0) (= (string-ref s 0) 45)) -1 1)) ; 45 = '-'
           (start (if (= sign -1) 1 0)))
      (let* ((go (lambda (i acc)
                   (if (>= i n)
                       acc
                       (let* ((c (string-ref s i))
                              (d (- c 48))) ; 48 = '0'
                         (go (+ i 1) (+ (* acc 10) d)))))))
        (* sign (go start 0))))))

; --- main function: find pairs that sum to target ---
(defun sum-two
  (lambda (vec target)
    (let* ((n (vector-length vec))
           (pairs nil))
      ((lambda (self) (self self 0))
       (lambda (self i)
         (if (< i n)
             (begin
               ((lambda (self2) (self2 self2 (+ i 1)))
                (lambda (self2 j)
                  (if (< j n)
                      (begin
                        (if (= (+ (vector-ref vec i) (vector-ref vec j)) target)
                            (set! pairs (cons (list (vector-ref vec i)
                                                    (vector-ref vec j))
                                              pairs))
                            nil)
                        (self2 self2 (+ j 1)))
                      nil))))
               (self self (+ i 1)))
             pairs))))))

; --- entry point ---
(defun main
  (lambda ()
    (let* ((lines (read-lines "values.txt")))
      (if (or (null? lines) (null? (cdr lines)))
          (begin
            (print "Error: values.txt must contain at least two lines.")
            nil)
          (let* ((numbers-line (car lines))
                 (target-line (car (cdr lines)))
                 (nums (parse-csv-line numbers-line)))
            (if (<= (length nums) 1)
                (begin
                  (print "Error: first line must contain more than 1 number.")
                  nil)
                (let* ((vec (list-to-vector nums))
                       (target (string->number target-line))
                       (res (sum-two vec target)))
                  (begin
                    (write-lines "result.txt"
                                 (map (lambda (p)
                                        (string-append "[" 
                                                       (number->string (car p)) 
                                                       "," 
                                                       (number->string (car (cdr p))) 
                                                       "]"))
                                      (reverse res)))
                    (print "Result written to result.txt")
                    res)))))))))

; --- convert number back to string ---
(defun number->string
  (lambda (n)
    (if (< n 0)
        (string-append "-" (int->str (- n)))
        (int->str n))))

; Run main automatically
(main)
