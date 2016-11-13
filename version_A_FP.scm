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

;;; task 3
; Да се напише функция (checkMatrix? m k) която проверява дали на всеки ред 
; в дадена матрица m от цели числа има поне по едно число, кратно на k.

; Example: (checkMatrix? ‘((1 2 6) (3 8 9) (10 12 11)) 3) → #t
; Example: (checkMatrix? ‘((1 2 4) (3 8 9) (10 12 11)) 3) → #f

(define (kratno-of-list k l)
  (cond
    ((null? l) #f)
    ((= (remainder (car l) k) 0) #t)
    (else (kratno-of-list k (cdr l)))
  )
)

(define (checkMatrix? m k)
  (foldr (lambda (x y) (and x y)) #t (my-map (lambda (l) (kratno-of-list k l)) m))
)

;;; task 4
; Да се напише функция (longestDescending­ l), която намира низходящо сортиран 
; подсписък на списъка от числа l с максимална дължина. 
; Ако съществуват няколко такива подсписъка, функцията да върне първия отляво надясно.
; Упътване: Реализирайте помощна функция, която намира най-дългия низходящо сортиран префикс на даден списък.

; Example: (longestDescending­ ‘(5 3 8 6 4 2 6 7 1)) → (8 6 4 2)
; Example: (longestDescending­ ‘(1 2 3 4 5 6)) → (1)

(define (desc-pref l)
  (define (helper last-elem l)
    (cond
      ((null? l) '())
      ((< (car l) last-elem) (cons (car l) (helper (car l) (cdr l))))
      (else '())
    )
  )
  (if (null? l)
      '()
      (cons (car l) (helper (car l) (cdr l)))
  )
)

(define (longestDescending l)
  (define (helper max-desc-list size l)
    (cond
      ((null? l) max-desc-list)
      ((< (length max-desc-list) (length (desc-pref l))) (helper (desc-pref l) (length (desc-pref l)) (cdr l)))
      (else (helper max-desc-list size (cdr l)))
    )
  )
  (helper (desc-pref l) (length (desc-pref l)) l)
)