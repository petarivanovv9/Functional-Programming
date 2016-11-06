;;; (map f lst)

(define (my-map f l)
  (if (null? l)
      '()
     (cons (f (car l)) (my-map f (cdr l)))
  )
)

; > (my-map (lambda (x) (* x x)) '(1 2 3 4))
; (1 4 9 16)

;;; (filter pred lst)

(define (my-filter pred? l)
  (cond
    ((null? l) '())
    ((not (pred? (car l))) (my-filter pred? (cdr l)))
    (else (cons (car l) (my-filter pred? (cdr l))))
  )
)

; > (my-filter odd? '(1 2 3 4 5 6 7 8 9))
; (1 3 5 7 9)
; > (my-filter even? '(1 2 3 4 5 6 7 8 9))
; (2 4 6 8)
; > (my-filter (lambda (x) x) '(1 2 3 4 5 6 7 8 9))
; (1 2 3 4 5 6 7 8 9)