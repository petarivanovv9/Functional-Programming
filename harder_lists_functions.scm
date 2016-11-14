;;; (map f lst)

(define (my-map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (my-map f (cdr l)))
  )
)

; > (my-map (lambda (x) (* x x)) '(1 2 3 4))
; (1 4 9 16)

;;; (filter pred lst)

(define (my-filter pred? l)
  (cond
    ((null? l) '())
    ((not (pred? (car l))) (my-filter pred? (cdr l)))
    (else (cons (car l) (my-filter pred? (cdr l))))
  )
)

; > (my-filter odd? '(1 2 3 4 5 6 7 8 9))
; (1 3 5 7 9)
; > (my-filter even? '(1 2 3 4 5 6 7 8 9))
; (2 4 6 8)
; > (my-filter (lambda (x) x) '(1 2 3 4 5 6 7 8 9))
; (1 2 3 4 5 6 7 8 9)

;;; (remove lst val) - remove all val occurrences in the list

(define (my-remove l val)
  (cond
    ((null? l) '())
    ((= (car l) val) (my-remove (cdr l) val))
    (else (cons (car l) (my-remove (cdr l) val)))
  )
)

; > (my-remove (list 1 2 1 3 1 4) 1)
; (2 3 4)
; > (my-remove (list 1 2 1 3 1 4) 5)
; (1 2 1 3 1 4)

;;; selection sort

(define (min-elem l)
  (cond
    ((null? (cdr l)) (car l))
    ((< (car l) (min-elem (cdr l))) (car l))
    (else (min-elem (cdr l)))
  )
)

; > (min-elem (list 1 2 3 4 5 0))
; 0

(define (remove-first-min-elem l elem)
  (if (null? l)
      '()
      (if (= (car l) elem)
          (cdr l)
          (cons (car l) (remove-first-min-elem (cdr l) elem))
      )
  )
)

; > (remove-first-min-elem (list 1 2 3 1 4 5) (min-elem (list 1 2 3 1 4 5)))
; (2 3 1 4 5)

(define (sel-sort l)
  (if (null? l)
      '()
      (cons (min-elem l) (sel-sort (remove-first-min-elem l (min-elem l))))
  )
)

; > (sel-sort '(7 6 5 4 3 2 1))
; (1 2 3 4 5 6 7)
; > (sel-sort (list 1 2 3 8 6 5 0 8 9 6 7 6 5))
; (0 1 2 3 5 5 6 6 6 7 8 8 9)
; > (sel-sort (list 1 2 3 4))
; (1 2 3 4)


;;; Helpul functions

(define (all? pred? l)
  (cond
    ((null? l) #t)
    ((not (pred? (car l))) #f)
    (else (all? pred? (cdr l)))
  )
)

(define (any? pred? l)
  (not (all? (lambda (x) (not (pred? x))) l))
)
