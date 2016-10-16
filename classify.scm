; Проверка къде попада точка в координатната равнина
(define (classify x y)
  (if (< x 0)
    (if (< y 0)
      3
      2
    )
    (if (< y 0)
      4
      1
    )
  )
)

; Examples
; > (classify -2 -4)
; 3
; > (classify -2 4)
; 2
; > (classify 2 -4)
; 4
; > (classify 2 4)
; 1
