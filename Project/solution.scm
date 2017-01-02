;(define (getRandomSentence))

(define temp 
  (list 
    (list "към небето" "д") (list "куче" "п" "ср") (list "лае" "с" "ед") 
    (list "птиците" "д") (list "зелените" "о" "мн") (list "жаби" "п" "мн") 
    (list "гледат" "с" "мн") (list "голямото" "о" "ср")
  )
)

(define (my-filter pred? l)
  (cond
    ((null? l) '())
    ((not (pred? (car l))) (my-filter pred? (cdr l)))
    (else (cons (car l) (my-filter pred? (cdr l))))
  )
)

(define (my-map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (my-map f (cdr l)))
  )
)

(define (extractNouns l)
  (define (helper l res)
    (cond
      ((null? l) res)
      ((string=? (cadr (car l)) "п") (helper (cdr l) (cons (car l) res))) 
      (else (helper (cdr l) res))
    )
  )
  (helper l '())
)

(define (extractAdjectives l)
  (my-filter (lambda (x) (if (string=? (cadr x) "о") #t #f)) l))

(define (extractVerbs l)
  (my-filter (lambda (x) (if (string=? (cadr x) "с") #t #f)) l))

(define (extractAdverbs l)
  (my-filter (lambda (x) (if (string=? (cadr x) "д") #t #f)) l))

