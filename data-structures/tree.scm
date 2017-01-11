(define temp '(1 (2 () ()) (3 (4 () ()) (5 () ()))))

; "стандартни" функции за работа с дървета
(define (tree? t)
  (or (null? t)
      (and (list? t) (= (length t) 3))
      (tree? (cadr t))
      (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

; примерно дърво, над което ще тестваме
(define test
  (make-tree 3
             (make-tree 1
                        (make-leaf 2)
                        empty-tree)
             (make-tree 5
                        (make-leaf 9)
                        (make-leaf 3))))

(define (tree-height t)
  (if (empty-tree? t)
      -1
      (+ 1 (max (tree-height (left-tree t))
                (tree-height (right-tree t))))))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (range a b)
  (accumulate cons '() a b (lambda (x) x) (lambda (x) (+ 1 x))))

(define (foldr op nv l)
  (if (null? l)
      nv
     (op (car l) (foldr op nv (cdr l)))))


; task 1
(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (tree-sum (left-tree t))
         (tree-sum (right-tree t)))))

; task 2
(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t)
           (tree-max (left-tree t))
           (tree-max (right-tree t)))))

; task 3
(define (tree-level k t)
  (cond
    ((empty-tree? t) '())
    ((= k 0) (list (root-tree t)))
    (else (append (tree-level (- k 1) (left-tree t))
                  (tree-level (- k 1) (right-tree t))))))

; task 4
(define (all-levels t)
  (let ((height (tree-height t)))
    (map (lambda (i) (tree-level i t)) (range 0 (+ 1 height)))))

; task 5
(define (tree-map f t)
  (if (empty-tree? t)
      empty-tree
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

; task 6
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))

; task 7
(define (bst-insert val t)
  (cond
    ((empty-tree? t) (make-leaf val))
    ((< val (root-tree t)) (make-tree (root-tree t)
                                      (bst-insert val (left-tree t))
                                      (right-tree t)))
    (else (make-tree (root-tree t)
                     (left-tree t)
                     (bst-insert val (right-tree t))))))

; вмъкване на списък от стойности в дърво
(define (list->tree lst)
  (foldr bst-insert empty-tree lst))

; task 8
(define (tree-sort l) (tree->list (list->tree l)))
