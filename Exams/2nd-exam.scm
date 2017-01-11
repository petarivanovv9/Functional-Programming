;task 4

(define empty-tree '())
(define empty-tree? null?)
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define (leaf? root)
  (if (and (empty-tree? (left-tree root)) (empty-tree? (right-tree root)))
      #t
      #f))

(define (bloom t)
  (if (leaf? t)
      (make-tree (root-tree t)
                 (make-leaf (root-tree t))
                 (make-leaf (root-tree t)))
      (make-tree (root-tree t)
                 (bloom (left-tree t))
                 (bloom (right-tree t)))))