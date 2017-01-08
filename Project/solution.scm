(#%require (only racket/base random))
(#%require (only racket/base string-titlecase))
;(#%require (only racket/base read-string))
;(#%require (only racket/base read-bytes))


(define temp '(("към небето" "д") ("куче" "п" "ср") ("лае" "с" "ед")
               ("птиците" "д") ("зелените" "о" "мн") ("жаби" "п" "мн")
               ("гледат" "с" "мн") ("голямото" "о" "ср")))

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
  (my-filter (lambda (x) (if (string=? (cadr x) "п") #t #f)) l))

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
    (cond ( (= n 0) (car l) )
          ( else (nth-elem (- n 1) (cdr l))) )))


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


(define getRandomSentence-format1
  (lambda ()
    (define noun (getNoun))
    (define adjective (getAdjective))
    (define verb (getVerb))
    (define adverb (getAdverb))

    (if (and (check-adj-noun adjective noun) (check-noun-verb noun verb))
        (apply string-append "" (append (list (string-titlecase (car adjective)) " " (car noun)) (list " " (car verb) " " (car adverb) ".")))
        (getRandomSentence-format1)
    )
  )
)

(define getRandomSentence-format2
  (lambda ()
    (define two-nouns (cons (getNoun) (getNoun)))
    (define two-adjectives (cons (getAdjective) (getAdjective)))
    (define verb (getVerb))
    (define adverb (getAdverb))

    (if (and
          (and (not (string=? (car (car two-nouns)) (car (cdr two-nouns))))
               (and (check-adj-noun (car two-adjectives) (car two-nouns)) (check-adj-noun (cdr two-adjectives) (cdr two-nouns)))
          )
          (string=? (caddr verb) "мн"))
        (apply string-append "" (append
                                 (list (string-titlecase (car (car two-adjectives))) " " (car (car two-nouns)) " и " (car (cdr two-adjectives)) " " (car (cdr two-nouns)))
                                 (list " " (car verb) " " (car adverb) ".")))
        (getRandomSentence-format2)
    )
  )
)

(define getRandomSentence
  (lambda ()
    (if (= (random 2) 0)
        (getRandomSentence-format1)
        (getRandomSentence-format2)
    )
  )
  
          ; (with-output-to-file
        ;     "result.txt"
        ;     (lambda ()
        ;       (write
        ;         (apply string-append "" (append (list (string-titlecase (car adjective)) " " (car noun)) (list " " (car verb) " " (car adverb) ".")))
        ;       )
        ;     )
        ;     #:exists `update
        ; )
)