; Бързо степенуване
(define (pow x y)
  (if (= y 0)
    1
    (if (= (remainder y 2) 0)
      (pow (* x x) (quotient y 2))
      (* x (pow x (- y 1)))
    )
  )
)

; Examples
; > (pow 2 3)
; 8
; > (pow 2 10)
; 1024
; > (pow 3 2)
; 9
