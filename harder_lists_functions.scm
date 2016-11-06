;;; (map f lst)

(define (my-map f l)
  (if (null? l)
      '()
     (cons (f (car l)) (my-map f (cdr l)))
  )
)

; > (my-map (lambda (x) (* x x)) '(1 2 3 4))
; (1 4 9 16)


