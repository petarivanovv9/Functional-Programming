; Linear recursion

(define (sum n)
  (if (zero? n)
      0
      (+ n (sum (- n 1)))
  )
)

(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))
  )
)

; Tail recursion

(define (sum-2 n)
  (define (sum-iter n s)
     (if (zero? n)
         s
         (sum-iter (- n 1) (+ s n))
     )
  )
  
  (sum-iter n 0)
)

(define (fact-2 n)
  (define (fact-iter n x)
    (if (zero? n)
        x
        (fact-iter (- n 1) (* x n))
    )
  )
  
  (fact-iter n 1)
)











