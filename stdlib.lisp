; ============================================================================
; Mini Lisp — Small Standard Library (consolidated)
; How to use: ./minilisp stdlib.lisp myprog.lisp
; ============================================================================

; ----------------------------
; Control utilities (thunked)
; ----------------------------

(defun when-fn
  (lambda (cond th)
    (if cond (th) nil)))

(defun unless-fn
  (lambda (cond th)
    (if (not cond) (th) nil)))

; cond* con hasta 4 pares (pred, thunk) + else (thunk)
(defun cond*
  (lambda (p1 t1 p2 t2 p3 t3 p4 t4)
    (if (p1)
        (t1)
        (if (p2)
            (t2)
            (if (p3)
                (t3)
                (if (p4) (t4) nil))))))

; ----------------------------
; List helpers
; ----------------------------

(defun append
  (lambda (a b)
    (if (null? a) b (cons (car a) (append (cdr a) b)))))

(defun append1
  (lambda (xs y)
    (if (null? xs)
        (cons y nil)
        (cons (car xs) (append1 (cdr xs) y)))))

(defun reverse
  (lambda (xs)
    (let* ((go (lambda (xs acc)
                 (if (null? xs)
                     acc
                     (go (cdr xs) (cons (car xs) acc))))))
      (go xs nil))))

(defun last
  (lambda (xs)
    (if (null? xs)
        nil
        (if (null? (cdr xs)) (car xs) (last (cdr xs))))))

(defun take
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        nil
        (cons (car xs) (take (- n 1) (cdr xs))))))

(defun drop
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        xs
        (drop (- n 1) (cdr xs)))))

(defun range
  (lambda (start end)
    (if (>= start end)
        nil
        (cons start (range (+ start 1) end)))))

(defun map
  (lambda (f xs)
    (if (null? xs)
        nil
        (cons (f (car xs)) (map f (cdr xs))))))

(defun filter
  (lambda (p xs)
    (if (null? xs)
        nil
        (let* ((x (car xs))
               (rest (filter p (cdr xs))))
          (if (p x) (cons x rest) rest)))))

(defun foldl
  (lambda (f acc xs)
    (if (null? xs)
        acc
        (foldl f (f acc (car xs)) (cdr xs)))))

(defun foldr
  (lambda (f z xs)
    (if (null? xs)
        z
        (f (car xs) (foldr f z (cdr xs))))))

(defun zip
  (lambda (xs ys)
    (if (or (null? xs) (null? ys))
        nil
        (cons (cons (car xs) (cons (car ys) nil))
              (zip (cdr xs) (cdr ys))))))

; ----------------------------
; Vector helpers
; ----------------------------

(defun vector-to-list
  (lambda (v)
    (let* ((n (vector-length v))
           (go (lambda (i acc)
                 (if (< i 0)
                     acc
                     (go (- i 1) (cons (vector-ref v i) acc))))))
      (go (- (vector-length v) 1) nil))))

(defun list-to-vector
  (lambda (xs)
    (let* ((n (length xs))
           (v (make-vector n nil))
           (go (lambda (i ys)
                 (if (null? ys)
                     v
                     (begin
                       (vector-set! v i (car ys))
                       (go (+ i 1) (cdr ys)))))))
      (go 0 xs))))

(defun vector-map
  (lambda (f v)
    (let* ((n (vector-length v))
           (out (make-vector n nil))
           (go (lambda (i)
                 (if (< i n)
                     (begin
                       (vector-set! out i (f (vector-ref v i)))
                       (go (+ i 1)))
                     out))))
      (go 0))))

; ----------------------------
; String helpers
; ----------------------------

(defun join
  (lambda (sep xs)
    (let* ((build
            (lambda (xs acc)
              (if (null? xs)
                  acc
                  (if (null? (cdr xs))
                      (string-append acc (car xs))
                      (build (cdr xs)
                             (string-append acc (string-append (car xs) sep))))))))
      (if (null? xs) ""
        (build (cdr xs) (car xs))))))

; Split por separador de 1 carácter (entero: código ASCII)
(defun split1
  (lambda (s ch)
    (let* ((n (string-length s))
           (go (lambda (i buf out)
                 (if (>= i n)
                     (reverse (cons buf out))
                     (let* ((c (string-ref s i)))
                       (if (= c ch)
                           (go (+ i 1) "" (cons buf out))
                           (go (+ i 1)
                               (string-append buf (substring s i (+ i 1)))
                               out)))))))
      (go 0 "" nil))))

; ----------------------------
; Integer -> string (para usar números como claves en hash)
; ----------------------------

(defun int->str
  (lambda (n)
    (let* ((digits "0123456789")
           (build (lambda (x)
                    (if (< x 10)
                        (substring digits x (+ x 1))
                        (string-append (build (/ x 10))
                                       (substring digits (% x 10) (+ (% x 10) 1)))))))
      (if (< n 0)
          (string-append "-" (build (- n)))
          (build n)))))

; ----------------------------
; Hash helpers
; ----------------------------

(defun hash-update!
  (lambda (h k f default)
    (hash-set! h k (f (hash-ref h k default)))))

(defun hash-incr!
  (lambda (h k)
    (hash-update! h k (lambda (x) (+ x 1)) 0)))

(defun hash-from-alist
  (lambda (alist)
    (let* ((h (make-hash)))
      (foldl (lambda (acc p)
               (begin
                 (hash-set! h (car p) (car (cdr p)))
                 acc))
             h
             alist)
      h)))

; ----------------------------
; Algorithms / examples
; ----------------------------

; Tabla de frecuencias para vector de strings
(defun freq-table
  (lambda (vec)
    (let* ((n (vector-length vec))
           (h (make-hash)))
      ((lambda (self) (self self 0))
       (lambda (self i)
         (if (< i n)
             (begin
               (hash-incr! h (vector-ref vec i))
               (self self (+ i 1)))
             h))))))

; Two-sum: nums[i] + nums[j] = target (retorna (i j) o nil).
; Usa claves string para el hash (int->str).
(defun two-sum
  (lambda (vec target)
    (let* ((n (vector-length vec))
           (seen (make-hash)))
      ((lambda (self) (self self 0))
       (lambda (self i)
         (if (< i n)
             (let* ((x (vector-ref vec i))
                    (need (- target x))
                    (kj (int->str need))
                    (k  (int->str x))
                    (j (hash-ref seen kj -1)))
               (if (>= j 0)
                   (list j i)
                   (begin
                     (hash-set! seen k i)
                     (self self (+ i 1)))))
             nil))))))

; Memoize para f: int -> any  (usa clave string)
(defun memoize
  (lambda (f)
    (let* ((cache (make-hash)))
      (lambda (x)
        (let* ((k (int->str x))
               (v (hash-ref cache k nil)))
          (if v
              v
              (begin
                (hash-set! cache k (f x))
                (hash-ref cache k nil))))))))

; Fibonacci memoizado
(defun fib
  (let* ((mf (make-hash)))
    (lambda (n)
      (if (<= n 1)
          n
          (let* ((k (int->str n))
                 (v (hash-ref mf k nil)))
            (if v
                v
                (begin
                  (hash-set! mf k (+ (fib (- n 1)) (fib (- n 2))))
                  (hash-ref mf k nil))))))))


; ----------------------------
; File I/O helpers (requieren builtins C: read-file, write-file, append-file)
; ----------------------------

(defun read-lines
  (lambda (path)
    (split1 (read-file path) 10)))  ; 10 == '\n'

(defun write-lines
  (lambda (path xs)
    (write-file path (join "\n" xs))))

(defun append-line
  (lambda (path s)
    (append-file path (string-append s "\n"))))
