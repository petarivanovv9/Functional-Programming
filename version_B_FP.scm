;;; Примерна тема за първо контролно по ФП
;;; Вариант Б

;;; task 1

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))
  )
)

(define (mixed?-helper-1 f g a b)
  (accumulate + 0 a b
              (lambda (i) (if (< (f i) (g i)) 
                              1
                              0
                          )
              )
              (lambda (x) (+ 1 x)))
)

(define (mixed?-helper-2 f g a b)
  (accumulate + 0 a b
              (lambda (i) (if (> (f i) (g i)) 
                              1
                              0
                          )
              )
              (lambda (x) (+ 1 x)))
)

(define (mixed? f g a b)
  (and (>= (mixed?-helper-1 f g a b) 1) (>= (mixed?-helper-2 f g a b) 1))
)

