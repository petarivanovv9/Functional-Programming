(#%require (only racket/base random))
(#%require (only racket/base string-titlecase))
;(#%require (only racket/base read-string))
;(#%require (only racket/base read-bytes))


;(define temp 
;  (list 
;    (list "към небето" "д") (list "куче" "п" "ср") (list "лае" "с" "ед") 
;    (list "птиците" "д") (list "зелените" "о" "мн") (list "жаби" "п" "мн") 
;    (list "гледат" "с" "мн") (list "голямото" "о" "ср")))

(define temp '(("към небето" "д") ("куче" "п" "ср") ("лае" "с" "ед") 
               ("птиците" "д") ("зелените" "о" "мн") ("жаби" "п" "мн") 
               ("гледат" "с" "мн") ("голямото" "о" "ср")))

;(define temp 
;  (list 
;    (list "към небето" "д") (list "куче" "п" "ср") (list "лае" "с" "ед") (list "зелените" "о" "мн")))

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

(define on-of (lambda (l) (random-elem l)))

(define random-elem
  (lambda (l) (nth-elem (random (length l)) l)))

(define nth-elem
  (lambda (n l)
    (cond ( (= n 0) (car l))
          ( else (nth-elem (- n 1) (cdr l))))))


; reading input
;(define temp (call-with-input-file "input.txt" read))
;(define temp (file->lines "input.txt"))


(define nouns (extractNouns temp))
(define adjectives (extractAdjectives temp))
(define adverbs (extractAdverbs temp))
(define verbs (extractVerbs temp))

(define getNoun (lambda () (on-of nouns)))
(define getAdjective (lambda () (on-of adjectives)))
(define getAdverb (lambda () (on-of adverbs)))
(define getVerb (lambda () (on-of verbs)))

(define (check-adj-noun adj noun)
  (if (string=? (caddr adj) (caddr noun))
      #t #f))

;idea
;(define singular (....))
;(member singular ... and (caddr verb))

(define (check-noun-verb noun verb)
  (if 
   (or (and (or (or (string=? (caddr noun) "м") (string=? (caddr noun) "ж")) (string=? (caddr noun) "ср")) (string=? (caddr verb) "ед"))
       (and (string=? (caddr noun) "мн") (string=? (caddr verb) "мн"))
   )
      #t
      #f
  )
)


(define getRandomSentence
  (lambda ()
    (define noun (getNoun))
    (define adjective (getAdjective))
    (define verb (getVerb))
    (define adverb (getAdverb))

    (if (and (check-adj-noun adjective noun) (check-noun-verb noun verb))
        ;(apply string-append "" (append (list (string-titlecase (car adjective)) " " (car noun)) (list " " (car verb) " " (car adverb) ".")))
        ;(call-with-input-file some-file (lambda (out) (write "hello" out)))
        (with-output-to-file 	 
            "result.txt"
            (lambda () (write (apply string-append "" (append (append (list (string-titlecase (car (list (car adjective) " " (car noun))))) (cdr (list (car adjective) " " (car noun)))) (list " " (car verb) " " (car adverb) "."))) 
                                   
                                   ))
           #:exists `update
          )
        (getRandomSentence))
  )
)