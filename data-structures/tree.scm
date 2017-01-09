(define temp '(1 (2 () ()) (3 (4 () ()) (5 () ()))))

; "стандартни" функции за работа с дърветас
(define (tree? t)
  (or (null? t)
      (and (list? t) (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))
  )
)
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
