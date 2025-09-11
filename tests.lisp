; ============================================================================
; Mini Lisp – Friendly Unit Tests (Phases 1..6)
; Run: ./minilisp tests.lisp
; ============================================================================

; --- Tiny test harness (no let/while; pass thunks) --------------------------

(defun *pass* 0)
(defun *fail* 0)

(defun ok
  (lambda (label)
    (set! *pass* (+ *pass* 1))
    (print (string-append "[OK] " label))))

(defun fail-lines
  (lambda (label expected actual)
    (set! *fail* (+ *fail* 1))
    (print (string-append "[FAIL] " label))
    (print (string-append "  expected: "))
    (print expected)
    (print (string-append "       got: "))
    (print actual)))

; truthy
(defun assert-true
  (lambda (label th)
    (if (th)
        (ok label)
        (fail-lines label '#t '#f))))

; falsy
(defun assert-false
  (lambda (label th)
    (if (not (th))
        (ok label)
        (fail-lines label '#f '#t))))

; deep structural equality via equal?
(defun assert-equal
  (lambda (label expected thunk)
    ((lambda (actual)
       (if (equal? expected actual)
           (ok label)
           (fail-lines label expected actual)))
     (thunk))))

; string equality via string=?
(defun assert-string=
  (lambda (label expected thunk)
    ((lambda (actual)
       (if (string=? expected actual)
           (ok label)
           (fail-lines label expected actual)))
     (thunk))))

; ============================================================================
; Numbers & arithmetic
; ============================================================================

(assert-equal "add"            6  (lambda () (+ 1 2 3)))
(assert-equal "sub unary"      -5 (lambda () (- 5)))
(assert-equal "sub chain"      -4 (lambda () (- 1 2 3)))
(assert-equal "mul"            24 (lambda () (* 2 3 4)))
(assert-equal "div"            2  (lambda () (/ 20 5 2)))
(assert-equal "mod"            1  (lambda () (% 10 3)))
(assert-equal "abs pos"        7  (lambda () (abs 7)))
(assert-equal "abs neg"        7  (lambda () (abs -7)))
(assert-equal "max"            9  (lambda () (max 1 9 3 7)))
(assert-equal "min"            1  (lambda () (min 1 9 3 7)))

; ============================================================================
; Comparisons
; ============================================================================

(assert-true  "=: chain true"  (lambda () (= 3 3 3)))
(assert-false "=: chain false" (lambda () (= 3 2 3)))
(assert-true  "< chain"        (lambda () (< 1 2 3 4)))
(assert-true  "<= chain"       (lambda () (<= 1 1 2 2)))
(assert-true  "> chain"        (lambda () (> 4 3 2 1)))
(assert-true  ">= chain"       (lambda () (>= 4 4 3 3)))

; ============================================================================
; Logic & special forms
; ============================================================================

(assert-equal "if true branch"    42 (lambda () (if #t 42 0)))
(assert-equal "if false branch"    0 (lambda () (if #f 42 0)))
(assert-equal "begin returns last" 7  (lambda () (begin 1 2 7)))

(assert-equal "and returns last truthy" 3  (lambda () (and #t 1 2 3)))
(assert-equal "and stops on #f"         #f (lambda () (and #t #f 9)))

; note: 0 is truthy in this Lisp
(assert-equal "or returns first truthy" 0  (lambda () (or #f nil 0 7)))
(assert-equal "or all falsy → #f"       #f (lambda () (or #f nil #f)))

; ============================================================================
; Definitions, lambdas, closures, set!
; ============================================================================

(defun inc (lambda (x) (+ x 1)))
(assert-equal "defun+lambda call"  11 (lambda () (inc 10)))

; counter closure
(defun make-counter
  (lambda ()
    (defun n 0)
    (lambda ()
      (set! n (+ n 1))
      n)))

(defun c1 (make-counter))
(defun c2 (make-counter))
(assert-equal "counter c1 #1" 1 (lambda () (c1)))
(assert-equal "counter c1 #2" 2 (lambda () (c1)))
(assert-equal "counter c2 #1" 1 (lambda () (c2)))

; compose
(defun compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(defun square (lambda (x) (* x x)))
(defun inc-then-square (compose square inc))
(assert-equal "compose inc→square" 81 (lambda () (inc-then-square 8)))

; ============================================================================
; Lists
; ============================================================================

(defun xs (list 1 2 3))
(assert-equal "list value"        '(1 2 3) (lambda () xs))
(assert-equal "car"               1        (lambda () (car xs)))
(assert-equal "cdr"               '(2 3)   (lambda () (cdr xs)))
(assert-equal "null? false"       #f       (lambda () (null? xs)))
(assert-equal "null? true"        #t       (lambda () (null? nil)))
(assert-equal "length"            3        (lambda () (length xs)))

(defun ys (cons 0 xs))
(assert-true  "equal? struct true"  (lambda () (equal? ys '(0 1 2 3))))
(assert-false "equal? struct false" (lambda () (equal? xs '(1 (2) 3))))

(assert-true  "eq? numbers equal"   (lambda () (eq? 7 7)))
(assert-false "eq? pairs not id"    (lambda () (eq? xs ys)))

; ============================================================================
; Strings
; ============================================================================

(assert-string= "string literal" "hello" (lambda () "hello"))

(defun s1 "ab")
(defun s2 "cd")
(assert-string= "string-append" "abcd" (lambda () (string-append s1 s2)))
(assert-equal   "string-length" 4      (lambda () (string-length (string-append s1 s2))))

(assert-string= "substring middle" "bc" (lambda () (substring "abcd" 1 3)))
(assert-equal   "string-ref code a=97" 97 (lambda () (string-ref "abc" 0)))

(assert-true  "string=? true"   (lambda () (string=? "abc" "abc")))
(assert-false "string=? false"  (lambda () (string=? "abc" "abd")))
(assert-true  "string<? chain"  (lambda () (string<? "a" "b" "c")))
(assert-true  "string>? chain"  (lambda () (string>? "c" "b" "a")))
(assert-true  "string<=? eq"    (lambda () (string<=? "zz" "zz")))
(assert-true  "string>=? eq"    (lambda () (string>=? "zz" "zz")))

; ============================================================================
; Vectors (Phase 6)
; ============================================================================

; literal vector
(assert-equal "vector literal length"
              3
              (lambda ()
                (vector-length (vector 1 2 3))))

(assert-equal "vector-ref works"
              2
              (lambda ()
                (vector-ref (vector 1 2 3) 1)))

; make-vector
(assert-equal "make-vector fills slots"
              0
              (lambda ()
                (vector-ref (make-vector 3 0) 2)))

(assert-equal "make-vector length"
              5
              (lambda ()
                (vector-length (make-vector 5 "x"))))

; vector-set!
(defun v (vector 1 2 3))
(vector-set! v 1 42)
(assert-equal "vector-set! changes element"
              42
              (lambda ()
                (vector-ref v 1)))

; small algorithm: sum of vector (recursive, no while)
(defun sum-vector
  (lambda (vec)
    (begin
      (defun n (vector-length vec))
      (defun loop
        (lambda (i acc)
          (if (< i n)
              (loop (+ i 1) (+ acc (vector-ref vec i)))
              acc)))
      (loop 0 0))))

(assert-equal "sum-vector [1..5]"
              15
              (lambda () (sum-vector (vector 1 2 3 4 5))))


; ============================================================================
; Let / Let*
; ============================================================================

; Basic let: parallel bindings, body returns last form
(assert-equal "let basic sum"
              3
              (lambda ()
                (let ((x 1) (y 2))
                  (+ x y))))

(assert-equal "let body returns last"
              7
              (lambda ()
                (let ((x 1))
                  (begin
                    (+ x 1)
                    (+ x 6)))))

; Parallel semantics: RHS are evaluated in the *outer* env
(defun x 100)
(assert-equal "let parallel uses outer x on RHS"
              100
              (lambda ()
                (let ((x 1) (y x))  ; y sees outer x=100, not the new x
                  y)))

; Sequential semantics in let*: each binding can see previous ones
(assert-equal "let* sequential uses newly bound x"
              1
              (lambda ()
                (let* ((x 1) (y x))
                  y)))

; Shadowing is local
(assert-equal "let shadowing is local"
              10
              (lambda ()
                (let ((x 10)) x)))
(assert-equal "outer x unchanged after let"
              100
              (lambda () x))

; A slightly larger example mixing let/let* and closures
(assert-equal "let and closure capture"
              9
              (lambda ()
                (let ((inc (lambda (z) (+ z 1))))
                  (let* ((a 4) (b (inc a)))
                    (+ a b)))))   ; 4 + 5 = 9

; ============================================================================
; Hash tables
; ============================================================================

; Make empty hash and size
(defun h (make-hash))
(assert-equal "hash-size empty" 0 (lambda () (hash-size h)))

; Set two keys and read them
(hash-set! h "a" 1)
(hash-set! h "b" 2)
(assert-equal "hash-size after 2 inserts" 2 (lambda () (hash-size h)))
(assert-equal "hash-ref a" 1 (lambda () (hash-ref h "a" -1)))
(assert-equal "hash-ref b" 2 (lambda () (hash-ref h "b" -1)))

; Default when missing
(assert-equal "hash-ref missing uses default" 999 (lambda () (hash-ref h "zzz" 999)))

; Overwrite should not change size
(hash-set! h "a" 111)
(assert-equal "overwrite keeps size" 2 (lambda () (hash-size h)))
(assert-equal "overwrite updates value" 111 (lambda () (hash-ref h "a" -1)))

; has-key? true/false
(assert-true  "hash-has-key? true"  (lambda () (hash-has-key? h "a")))
(assert-false "hash-has-key? false" (lambda () (hash-has-key? h "nope")))

; remove!
(assert-true  "hash-remove! removes existing" (lambda () (hash-remove! h "b")))
(assert-equal "size decreased after remove" 1 (lambda () (hash-size h)))
(assert-false "removed key no longer present" (lambda () (hash-has-key? h "b")))

; Frequency count from a vector of strings using a hash
(defun freq-count
  (lambda (vec)
    (let* ((n (vector-length vec))
           (m (make-hash)))
      ((lambda (self)               ; self-recursive helper
         (self self 0))             ; kick-off
       (lambda (self i)
         (if (< i n)
             (begin
               (let ((k (vector-ref vec i)))
                 (hash-set! m k (+ (hash-ref m k 0) 1)))
               (self self (+ i 1)))
             m))))) )               



(defun vwords (vector "a" "b" "a" "c" "b" "a"))
(defun ftab (freq-count vwords))

(assert-equal "freq a = 3" 3 (lambda () (hash-ref ftab "a" 0)))
(assert-equal "freq b = 2" 2 (lambda () (hash-ref ftab "b" 0)))
(assert-equal "freq c = 1" 1 (lambda () (hash-ref ftab "c" 0)))
(assert-equal "freq missing = 0 default" 0 (lambda () (hash-ref ftab "zzz" 0)))

; ============================================================================
; Print returns its argument (smoke)
; ============================================================================

(assert-equal "print returns value" 123 (lambda () (print 123)))

; ============================================================================
; Summary
; ============================================================================

(print "-------- SUMMARY --------")
(print (string-append "passed: ")) (print *pass*)
(print (string-append "failed: ")) (print *fail*)
(print (string-append "result: "))
(print (if (= *fail* 0) "OK ✅" "FAIL ❌"))
(print "-------------------------")
(print "NOTE: Friendly tests; no engine error lines should appear.")
