(define (member? x l)
  (if (member x l) #t #f))

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))

(define (my-filter pred? l)
  (cond ((null? l) '())
        ((not (pred? (car l))) (my-filter pred? (cdr l)))
        (else (cons (car l) (my-filter pred? (cdr l))))))

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


; task 3

; проверка за съдържание на път в граф
; (contains-path? '(a b f e) G) -> #t
; (contains-path? '(a c f e) G) -> #f
(define (contains-path? path g)
  (cond ((null? path) #t)
        ((null? (cdr path)) (member? (car path) (vertices g)))
        ((has-edge? (car path) (cadr path) g) (contains-path? (cdr path) g))
        (else #f)))


; task 4 - предшественици на връх

(define (predecessors v g)
  (filter (lambda (u) (has-edge? u v g)) (vertices g)))


; task 7 - списък с всичките ребра на граф

(define (edge-list g)
  (define (make-pairs-single l) (map (lambda (v) (cons (car l) v)) (cdr l)))
  (apply append (map make-pairs-single g)))


; task 8 - обръщане на всички ребра в граф

(define (invert g)
  (define (flip p) (cons (cdr p) (car p)))
  (make-from-edges (map flip (edge-list g))))


; task 9 - обхождане в широчина
; (bfs 'a G) -> '(a b c d e f g), случайна подредба

; използваме помощна ф-ия, която на всяка "итерация" пази текущото "ниво"
; и изчислява следващото с друга отделна ф-ия

(define (bfs v g)
  (define (get-next-level current visited)
    (apply append (map (lambda (v) (my-filter (lambda (v) (not (member? v visited)))
                                           (successors v g)))
                       current)))
  (define (helper current result)
    (let ((next (get-next-level current result)))
      (if (null? next)
          result
          (helper next (append result next)))))
  (helper (list v) (list v)))