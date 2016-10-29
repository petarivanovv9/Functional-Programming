; Tree recursion

(define (fib n)
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
)

; Fibonacci's tale (or tail)

(define (fib-2 n)
  (define (fib-iter n a0 a1)
    (cond 
        ((zero? n) a0)
        ((= n 1) a1)
        (else (fib-iter (- n 1) a1 (+ a0 a1)))
    )
  )
  (fib-iter n 0 1)
)