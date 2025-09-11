; Check if two strings are anagrams by comparing char counts
(defun valid-anagram?
  (lambda (s t)
    (let* ((ns (string-length s))
           (nt (string-length t)))
      (if (not (= ns nt))
          #f
          (let* ((h (make-hash))
                 (i 0)
                 (go1 (lambda (i)
                        (if (< i ns)
                            (let* ((c (string-ref s i))
                                   (k (int->str c)))
                              (hash-set! h k (+ (hash-ref h k 0) 1))
                              (go1 (+ i 1)))
                            nil)))
                 (go2 (lambda (i bad)
                        (if (or bad (>= i nt))
                            (not bad)
                            (let* ((c (string-ref t i))
                                   (k (int->str c))
                                   (cnt (hash-ref h k 0)))
                              (if (= cnt 0)
                                  (go2 (+ i 1) #t)
                                  (begin
                                    (hash-set! h k (- cnt 1))
                                    (go2 (+ i 1) #f))))))))
            (go1 0)
            (go2 0 #f))))))

; quick tests
(print (valid-anagram? "anagram" "nagaram")) ; => #t
(print (valid-anagram? "rat" "car"))         ; => #f
