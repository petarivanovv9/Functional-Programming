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

;;; task 2

(define (member? x l)
  (cond
    ((null? l) #f)
    ((= (car l) x) #t)
    (else (member? x (cdr l)))
  )
)

(define (filter pred? l)
  (cond
    ((null? l) '())
    ((pred? (car l)) (cons (car l) (filter pred? (cdr l))))
    (else (filter pred? (cdr l)))
  )
)

(define (filter-more-than-one l)
  (define (helper res l)
    (cond
      ((null? l) res)
      ((and (member (car l) (cdr l)) (not (member (car l) res))) (helper (cons (car l) res) (cdr l)))
      (else (helper res (cdr l)))
    )
  )
  (helper '() l)
)

(define (filter-unique lst-all)
  (define lst-more-than-one (filter-more-than-one lst-all))
  (define (helper res lst-all lst-more-than-one)
    (cond
      ((null? lst-all) res)
      ((member (car lst-all) lst-more-than-one) (helper res (cdr lst-all) lst-more-than-one))
      (else (helper (cons (car lst-all) res) (cdr lst-all) lst-more-than-one))
    )
  )
  (helper '() lst-all lst-more-than-one)
)

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))
  )
)

(define (max-of-list l)
  (if (null? l)
      #f
      (foldr max (car l) l)
  )
)

(define (maxUnique ll)
  (max-of-list 
    (filter 
       integer? 
       (map (lambda (l) (max-of-list (filter-unique l))) ll)
    )
  )
)

;;; task 3

(define (all-l-deliteli k l)
  (define (helper res k l)
    (cond
      ((null? l) res)
      ((not (= (remainder k (car l)) 0)) (helper (cons (car l) res) k (cdr l)))
      (else (helper res k (cdr l)))
    )
  )
  (null? (helper '() k l))
)

(define (checkMatrix m k)
 (not (foldr (lambda (x y) (or x y)) #f (map (lambda (l) (all-l-deliteli k l)) m)))
)

;;; task 4

(define (asc-pref l)
  (define (helper last-elem l)
    (cond
      ((null? l) '())
      ((< last-elem (car l)) (cons (car l) (helper (car l) (cdr l))))
      (else '())
    )
  )
  (if (null? l)
      '()
      (cons (car l) (helper (car l) (cdr l)))
  )
)

(define (longestAscending l)
  (define (helper max-asc-list size l)
    (cond
      ((null? l) max-asc-list)
      ((< (length max-asc-list) (length (asc-pref l))) (helper (asc-pref l) (length (asc-pref l)) (cdr l)))
      (else (helper max-asc-list size (cdr l)))
    )
  )
  (helper (asc-pref l) (length (asc-pref l)) l)
)