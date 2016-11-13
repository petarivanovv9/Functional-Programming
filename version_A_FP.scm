;;; Примерна тема за първо контролно по ФП
;;; Вариант А

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))
  )
)

;;; task 1
; Да се напише функция (meetTwice? f g a b), която проверява дали в целочисления 
; интервал [a; b] съществуват две различни цели числа x и y такива, че 
; f(x) = g(x) и f(y) = g(y).

; Example:(meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1) → #f
; Example:(meetTwice? (lambda(x)x) sqrt 0 5) → #t

(define (meetTwice?-helper f g a b)
  (accumulate + 0 a b 
              (lambda (i) 
                (if (= (f i) (g i))
                    1
                    0
                )
              )  
              (lambda (i) (+ 1 i)))
)

(define (meetTwice? f g a b)
  (>= (meetTwice?-helper f g a b) 2)
)