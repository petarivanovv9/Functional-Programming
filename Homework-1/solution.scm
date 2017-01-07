;;; task 1
(define (hailstone n)
  (define (helper n l)
    (cond
      ((= n 1) l)
      ((even? n) (helper (/ n 2) (cons (/ n 2) l)))
      (else (helper (+ (* 3 n) 1) (cons (+ (* 3 n) 1) l)))
    )
  )
  (reverse (helper n (cons n '())))
)