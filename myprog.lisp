(write-file "out.txt" "hello")
(append-line "out.txt" "world")
(print (file-exists? "out.txt"))  ; => #t
(print (read-lines "out.txt"))    ; => ("hello" "world")
