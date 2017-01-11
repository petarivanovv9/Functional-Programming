(define (member? x l)
  (if (member x l) #t #f))

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))

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


; task 2

; за да добавяме ребро първо трябва да добавим върховете му
(define (add-vertex v g)
  (if (member? v (vertices g))
      g
      (cons (list v) g)))

; добавяне на ребро в граф
(define (add-edge u v g)
  (if (has-edge? u v g)
      g
      (let ((newg (add-vertex u (add-vertex v g))))
        (map (lambda (l) (if (equal? (car l) u)
                             (append l (list v))
                             l))
             newg))))

; създаване на граф от цял списък с ребра
(define (make-from-edges l)
  (foldr (lambda (e g) (add-edge (car e)
                                 (cdr e)
                                 g))
         '()
         l))
