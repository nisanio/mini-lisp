; Binary Tree Level Order Traversal (BFS)
; Node = (node value left right), nil = empty

(defun make-node
  (lambda (v l r)
    (list 'node v l r)))

(defun node-val
  (lambda (n)
    (car (cdr n))))

(defun node-left
  (lambda (n)
    (car (cdr (cdr n)))))

(defun node-right
  (lambda (n)
    (car (cdr (cdr (cdr n))))))

; --- collect values of a list of nodes (one level) ---
(defun collect-vals-iter
  (lambda (lst acc)
    (if (null? lst)
        (reverse acc)
        (collect-vals-iter (cdr lst) (cons (node-val (car lst)) acc)))))

(defun collect-vals
  (lambda (nodes)
    (collect-vals-iter nodes nil)))

; --- collect children (next level) from a list of nodes ---
(defun collect-kids-iter
  (lambda (lst acc)
    (if (null? lst)
        (reverse acc)
        (let* ((n (car lst))
               (l (node-left n))
               (r (node-right n))
               (acc1 (if l (cons l acc) acc))
               (acc2 (if r (cons r acc1) acc1)))
          (collect-kids-iter (cdr lst) acc2)))))

(defun collect-kids
  (lambda (nodes)
    (collect-kids-iter nodes nil)))

; --- main BFS loop over levels ---
(defun level-loop
  (lambda (current acc)
    (if (null? current)
        (reverse acc)
        (let* ((vals (collect-vals current))
               (kids (collect-kids current)))
          (level-loop kids (cons vals acc))))))

(defun level-order
  (lambda (root)
    (if (null? root)
        nil
        (level-loop (list root) nil))))

; ----- demo tree -----
;       1
;      / \
;     2   3
;        / \
;       4   5

(defun tree
  (make-node 1
             (make-node 2 nil nil)
             (make-node 3
                        (make-node 4 nil nil)
                        (make-node 5 nil nil))))

(print (level-order tree))
; Expected: ((1) (2 3) (4 5))
