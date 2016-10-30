; Пример за функция от по-висок ред, пресмятаща определен интеграл

(define (integral f a b)
  (let ((h 0.00001))
    (if (>= a b)
        0
        (+ (* h (f (+ a (/ h 2)))) (integral f (+ a h) b))
    )
  )
)

(define (id x) x)
(define (sq x) (* x x))
(define (f x) 2)

; Examples
; > (integral id 3 4)
; 3.5000000000032734
; > (integral sin 3 4)
; -0.3363488757409859
; > (integral f 3 4)
; 1.9999999999961675