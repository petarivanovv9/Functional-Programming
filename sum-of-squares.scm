; Define a procedure that takes three numbers
; as arguments and returns the sum of the
; squares of the two larger numbers.

(define (f a b c)
  (define (sum-of-squares x y)
    (+ (* x x) (* y y))
  )
  
  (cond 
    ((and (> a c) (> b c)) (sum-of-squares a b))
    ((> a b) (sum-of-squares a c))
    (else (sum-of-squares b c))
  )
)