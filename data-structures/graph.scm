(define (member? x l)
  (if (member x l) #t #f))

; маък граф, над който ще тестваме всички функции
(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f)   ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))