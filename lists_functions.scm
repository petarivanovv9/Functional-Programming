;;; create list from A to B

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))
  )
)

; cons - Returns a newly allocated pair whose first element is a and second element is d.
; > (cons 1 2)
; '(1 . 2)
; > (cons 1 '())
; '(1)

(define (make-list a b)
  (if (> a b)
      a
      (accumulate cons '() a b (lambda (x) x) (lambda (x) (+ 1 x)))
  )
)

;;; lenght of list

; cdr - Returns the second element of the pair p.
; > (cdr '(1 2))
; '(2)
; > (cdr '(1))
; '()

(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))
  )
)

; > (len '(1 2 3 4 5) )
; 5

;;; reverse list

; car - Returns the first element of the pair p.
; > (car '(1 2))
; 1
; > (car (cons 2 3))
; 2

(define (rev l)
  (define (rev-acc l res)
    (if (null? l)
        res
        (rev-acc (cdr l) (cons (car l) res))
    )
  )
  (rev-acc l '())
)