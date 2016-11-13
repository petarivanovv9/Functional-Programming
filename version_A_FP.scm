;;; Примерна тема за първо контролно по ФП
;;; Вариант А

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))
  )
)

(define (id x) x)
(define (1+ x) (+ 1 x))

;;; task 1
; Да се напише функция (meetTwice? f g a b), която проверява дали в целочисления 
; интервал [a; b] съществуват две различни цели числа x и y такива, че 
; f(x) = g(x) и f(y) = g(y).

; Example: (meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1) → #f
; Example: (meetTwice? (lambda(x)x) sqrt 0 5) → #t

(define (meetTwice?-helper f g a b)
  (accumulate + 0 a b 
              (lambda (i) 
                (if (= (f i) (g i))
                    1
                    0
                )
              )  
              1+)
)

(define (meetTwice? f g a b)
  (>= (meetTwice?-helper f g a b) 2)
)

;;; task 2
; Да се напише функция (maxDuplicate ll), която по списък от списъци от цели числа ll 
; намира най-­голямото от тези числа, които се повтарят в рамките на списъка, в който се срещат. 
; Ако в нито един списък няма повтарящи се числа, функцията да връща #f.

; Example: (maxDuplicate ‘((1 2 3 2) (-­4 -­4) (5))) → 2
; Example: (maxDuplicate ‘((1 2 3) (-­4 -­5 -6) ())) → #f

; 1) обхождане на подсписъците
; 2) взимаме числото което се повтаря повече от веднъж във всеки един подсписък
; 3) ако няма число което се повтаря повече от веднъж -> #f
; 4) взимаме максималното число от всички подсписъци

(define (member? x l)
  (cond 
    ((null? l) #f)
    ((= x (car l)) #t)
    (else (member? x (cdr l)))
  )
)

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))
  )
)

(define (my-map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (my-map f (cdr l)))
  )
)

(define (filter-more-than-one l)
  (define (helper res l)
    (cond
      ((null? l) res)
      ((and (member? (car l) (cdr l)) (not (member? (car l) res))) (helper (cons (car l) res) (cdr l)))
      (else (helper res (cdr l)))
    )
  )
  (helper '() l)
)

(define (max-of-list lst)
  (if (null? lst)
      #f
      (foldr max (car lst) (cdr lst))
  )
)

(define (my-filter pred? l)
  (cond
    ((null? l) '())
    ((not (pred? (car l))) (my-filter pred? (cdr l)))
    (else (cons (car l) (my-filter pred? (cdr l))))
  )
)

(define (maxDuplicate ll)
  
  (max-of-list (my-filter integer? (my-map (lambda (x) (max-of-list (filter-more-than-one x))) ll)))
  
)