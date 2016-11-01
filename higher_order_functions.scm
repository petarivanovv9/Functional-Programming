(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (sum-e a b)
  (if (> a b)
      0
      (+ (/ 1 (fact a)) (sum-e (+ a 1) b))))

;;; Напишете функция от по-висок ред (sum term a next b), 
;;; която улавя общото от трите дефиниции и параметризира специфичното

(define (identity x) x)
(define (1+ x) (+ 1 x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))
  )
)

; > (sum identity 1 1+ 10)
; 55

(define (sum-integers a b)
  (sum identity a add1 b))

(define (sum-cubes a b)
  (sum (lambda (x) (* x x x)) a add1 b))

(define (sum-e a b)
  (sum (lambda (x) (/ 1 (fact x))) a 1+ b))

;;; problem 3

;(define (integral f a b delta)  
;
;)

;;; problem 4

(define (sum2 term a next b)
  (define (iter current result)
    (if (> current b)
  	result
  	(iter (next current) (term current result))
    )
  )
  (iter a 0)
)

; > (sum2 + 1 1+ 10)
; 55

;;; problem 7

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))
  )
)

(define (filtered-accumulate op nv pred term a next b)
  (cond 
    ((> a b) nv)
    ((not (pred a)) (filtered-accumulate op nv pred term (next a) next b))
    (else (op (term a) (filtered-accumulate op nv pred term (next a) next b)))
  )
)

(define (odd-2? x)
  (not (= (remainder x 2) 0))
)

; > (filtered-accumulate + 0 odd-2? identity 1 1+ 10)
; 25

;;; problem 9, (constantly c) that returns f(x) = c

(define (constantly c)
  (lambda (x) c)
)

(define forever-21 (constantly 21))
; > (forever-21 29)
; 21
; > (forever-21 21)
; 21

;;; problem 10

(define (flip f)
  (lambda (x y) (f y x))
)

(define cons^ (flip cons))
; > (cons^ 2 3)
; (3 . 2)

;;; problem 11
;;; функция (curry-3 f), която връща curried вариант на триместна функция f

(define (curry-3 f)
  (lambda (x) (lambda (y) (lambda (z) (f x y z))))
)

(define +. (curry-3 +))
; > (((+. 1) 2) 3)
; 6

;;; problem 12
;;; функция (compose f g), която връща функцията f∘g

(define (compose f g)
  (lambda (x) (f (g x)))
)

(define f (compose (lambda (x) (+ x 1)) (lambda (x) (* x x)))) ;  x^2 + 1
; > (f 4)
; 17
; > (f 7)
; 50

;;; problem 13
;;; функция (complement p), която приема предикат и връща неговото отрицание

(define (complement p)
  (lambda (x) (not (p x)))
)

(define (less-than-5? x) (< x 5))
(define f (complement less-than-5?))
; > (f 3)
; #f
; > (f 5)
; #t
; > (f 7)
; #t