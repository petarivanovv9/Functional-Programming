; (reverse-int n)

(define (reverse-int n)
  (define (rev-help n rev)
    (if (<= n 0)
        rev
        (rev-help (quotient n 10) (+ (* rev 10) (remainder n 10)))
    )
  )
  (rev-help n 0)
)

; Examples
; (reverse-int 1234) -> 4321
; (reverse-int 10000) -> 1

; (/ 617 5) -> 123 2/5
; (quotient 617 5) -> 123
