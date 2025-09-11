; ============================================================================
; program.lisp
; Reads numbers and a target from values.txt, finds all pairs that sum to target,
; and writes them into result.txt as a JSON-like array: [[a,b],[c,d],...]
; ============================================================================

; --- string trimming (spaces only) ------------------------------------------
(defun string-trim
  (lambda (s)
    (let* ((n (string-length s))
           (l (lambda (i)
                (if (>= i n) i
                    (let* ((c (string-ref s i)))
                      (if (or (= c 32) (= c 9) (= c 13) (= c 10))
                          (l (+ i 1))
                          i)))))
           (r (lambda (i)
                (if (< i 0) i
                    (let* ((c (string-ref s i)))
                      (if (or (= c 32) (= c 9) (= c 13) (= c 10))
                          (r (- i 1))
                          i))))))
      (let* ((i0 (l 0))
             (i1 (r (- n 1))))
        (if (> i0 i1) "" (substring s i0 (+ i1 1)))))))

; --- string to integer (supports optional leading '-') ----------------------
(defun string->number
  (lambda (s)
    (let* ((t (string-trim s))
           (n (string-length t))
           (neg (and (> n 0) (= (string-ref t 0) 45))) ; '-'
           (start (if neg 1 0)))
      (let* ((go (lambda (i acc)
                   (if (>= i n)
                       acc
                       (let* ((c (string-ref t i))
                              (d (- c 48)))          ; '0' -> 48
                         (go (+ i 1) (+ (* acc 10) d)))))))
        (if neg (- (go start 0)) (go start 0))))))

; --- format integer to string using stdlib's int->str -----------------------
(defun number->string
  (lambda (n)
    (if (< n 0)
        (string-append "-" (int->str (- n)))
        (int->str n))))

; --- parse CSV "1, 2, -3" -> (1 2 -3) --------------------------------------
(defun parse-csv-line
  (lambda (s)
    (let* ((parts (split1 s 44))          ; ',' = 44
           (clean (filter (lambda (x) (not (= (string-length (string-trim x)) 0))) parts)))
      (map (lambda (tok) (string->number tok)) clean))))


; Find all pairs (i<j) that sum to target using a hash of seen counts.
(defun sum-two
  (lambda (vec target)
    (let* ((n (vector-length vec))
           (seen (make-hash))  ; key: int->str(value) -> count seen so far
           (pairs nil))
      ((lambda (self) (self self 0))
       (lambda (self j)
         (if (< j n)
             (let* ((x (vector-ref vec j))
                    (need (- target x))
                    (kneed (int->str need))
                    (kx    (int->str x))
                    (cnt   (hash-ref seen kneed 0)))
               ; If we've seen 'need' before, add that many pairs (need, x)
               (if (> cnt 0)
                   (set! pairs (cons (list need x) pairs))
                   nil)
               ; Mark current x as seen
               (hash-set! seen kx (+ (hash-ref seen kx 0) 1))
               (self self (+ j 1)))
             (reverse pairs)))))))


; --- convert list of pairs to JSON-like string: [[a,b],[c,d],...] ----------
(defun pairs->json
  (lambda (pairs)
    (let* ((pair->str (lambda (p)
                        (string-append "["
                                       (number->string (car p)) ","
                                       (number->string (car (cdr p))) "]")))
           (parts (map pair->str pairs)))
      (string-append "[" (join "," parts) "]"))))

; --- main entry point -------------------------------------------------------
(defun main
  (lambda ()
    (let* ((lines (read-lines "values.txt")))
      (if (or (null? lines) (null? (cdr lines)))
          (begin
            (print "Error: values.txt must contain two lines.")
            nil)
          (let* ((numbers-line (car lines))
                 (target-line  (car (cdr lines)))
                 (nums (parse-csv-line numbers-line)))
            (if (<= (length nums) 1)
                (begin
                  (print "Error: first line must contain more than 1 number.")
                  nil)
                (let* ((vec (list-to-vector nums))
                       (target (string->number target-line))
                       (pairs (sum-two vec target))
                       (out   (pairs->json pairs)))
                  (begin
                    (write-file "result.txt" (string-append out "\n"))
                    (print "Result written to result.txt")
                    pairs))))))))

; run
(main)
