; (palindrome? n)

(define (reverse-int n)
  (define (rev-help n rev)
    (if (<= n 0)
        rev
        (rev-help (quotient n 10) (+ (* rev 10) (remainder n 10)))
    )
  )
  (rev-help n 0)
)

(define (palindrome? n)
  (= n (reverse-int n))
)

; Examples
; (palindrome? 12321) -> #t
; (palindrome? 4040) -> #f