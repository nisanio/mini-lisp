; Sliding window with last-seen index (hash with char-code keys)
(defun length-longest-unique-substr
  (lambda (s)
    (let* ((n (string-length s))
           (last (make-hash))      ; k = int->str(char), v = last index + 1
           (best 0)
           (start 0)
           (go (lambda (i)
                 (if (< i n)
                     (let* ((c (string-ref s i))
                            (k (int->str c))
                            (seen (hash-ref last k 0)))
                       (if (> seen 0)
                           (if (> seen start) (set! start seen) nil)
                           nil)
                       (hash-set! last k (+ i 1))
                       (if (> (+ (- i start) 1) best)
                           (set! best (+ (- i start) 1))
                           nil)
                       (go (+ i 1)))
                     best))))
      (go 0))))

; quick tests
(print (length-longest-unique-substr "abcabcbb")) ; => 3 ("abc")
(print (length-longest-unique-substr "bbbbb"))    ; => 1
(print (length-longest-unique-substr "pwwkew"))   ; => 3 ("wke")
