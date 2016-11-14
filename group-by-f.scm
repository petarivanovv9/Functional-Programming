; Да се напише функция (group-by-f f lst), която групира елементите на списъка lst 
; по стойността, която f връща за тях:

; подредбата няма значение
; Example (group-by-f even? '(1 2 3 4 5)) -> ((#f (1 3 5)) (#t (2 4))) 
; Example (group-by-f length '((1 2 3) (4) (5 6 7))) -> '((1 ((4))) (3 ((1 2 3) (5 6 7))))

(define (filter pred? l)
  (cond
    ((null? l) '())
    ((not (pred? (car l))) (filter pred? (cdr l)))
    (else (cons (car l) (filter pred? (cdr l))))
  )
)

; 1. '(1 2 3 4 5)
; -> '((1 . #f) (2 . #t) (3 . #f) (4 . #t) (5 . #f))
(define (make-pairs f l)
  (map (lambda (x) (cons x (f x))) l)
)

; 2. '((1 . #f) (2 . #t) (3 . #f) (4 . #t) (5 . #f))
; -> '( ((1 . #f) (3 . #f) (5 . #f))
;       ((2 . #t) (4 . #t)) )
(define (group-pairs l)
  (let*
      (
       (predicate (lambda (x) (equal? (cdr (car l)) (cdr x))))
       (firsts (filter predicate l))
       (rest (filter (lambda (x) (not (predicate x))) l))
      )
      (if (null? l)
          '()
          (cons firsts (group-pairs rest))
      )
  )
)