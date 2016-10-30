(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))
  )
)

(define (id x) x)
(define (1+ x ) (+ x 1))

; Examples
; > (accumulate + 0 1 10 id 1+)
; 55
; > (accumulate + 0 1 10 (lambda (x) x) (lambda (x) (+ 1 x)))
; 55

; Other examples with accumulate

; Factorial
(define (fact n)
  (accumulate * 1 1 n (lambda (x) x) (lambda (x) (+ 1 x)))
)