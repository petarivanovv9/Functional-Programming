(define (member? x l)
  (if (member x l) #t #f))

; малък граф, над който ще тестваме всички функции
(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f)   ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))


; task 1

; взимане на всички върхове в граф
; (vertices G) -> '(a b c d e f g)(has-edge? 'c 'e) -> #f
(define (vertices g)
  (map car g))

; взимане на всички наследници на даден връх в граф
; (successors 'c G) -> '(a d)
(define (successors v g)
  (let ((result (assoc v g)))
    (if result (cdr result) '())))

; проверка дали съществува ребро м/у два върха в граф
; (has-edge? 'c 'e G) -> #f
(define (has-edge? u v g)
  (member? v (successors u g)))
