#lang racket

(define (member? x l)
  (if (member x l) #t #f))

;;; Graphs

(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f)   ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))

(define (vertices g)
  (map car g))

(define (successors v g)
  (let [(result (assoc v g))]
    (if result (cdr result) '())))

(define (has-edge? u v g)
  (member? v (successors u g)))

(define (add-vertex v g)
  (if (member? v (vertices g))
      g
      (cons (list v) g)))

(define (add-edge u v g)
  (if (has-edge? u v g)
      g
      (let [(newg (add-vertex u (add-vertex v g)))]
        (map (lambda (l) (if (equal? (car l) u)
                             (append l (list v))
                             l))
             newg))))

(define (predecessors v g)
  (filter (lambda (u) (has-edge? u v g)) (vertices g)))

(define (edge-list g)
  (define (make-pairs-single l) (map (lambda (v) (cons (car l) v)) (cdr l)))
  (apply append (map make-pairs-single g)))


;;; Trees

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define general-tree '(1 (2 (2 () () ()) () ())
                         (3 () (5 () ()) ())
                         (5 () () (6 () ()()))))

(define (sumTree t)
  (if (null? t)
      0
      (+ (car t) (foldr + 0  (map sumTree (cdr t))))))

(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))