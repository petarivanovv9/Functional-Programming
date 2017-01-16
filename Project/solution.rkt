#lang racket

;(define temp '(("към небето" "д") ("куче" "п" "ср") ("лае" "с" "ед")
;               ("птиците" "д") ("зелените" "о" "мн") ("жаби" "п" "мн")
;               ("гледат" "с" "мн") ("голямото" "о" "ср")))

(define (member? x l)
  (if (member x l)
      #t #f))

(define (extractNouns l)
  (filter (lambda (x) (if (string=? (cadr x) "п") #t #f)) l))

(define (extractAdjectives l)
  (filter (lambda (x) (if (string=? (cadr x) "о") #t #f)) l))

(define (extractVerbs l)
  (filter (lambda (x) (if (string=? (cadr x) "с") #t #f)) l))

(define (extractAdverbs l)
  (filter (lambda (x) (if (string=? (cadr x) "д") #t #f)) l))

(define on-of (lambda (l) (random-elem l)))

(define random-elem
  (lambda (l) (nth-elem (random (length l)) l)))

(define nth-elem
  (lambda (n l)
    (cond [(= n 0) (car l)]
          [else (nth-elem (- n 1) (cdr l))])))

(define (makeLongAdverb adverb-list)
  (define (helper lst adverb)
    (if (null? (cdr lst))
        adverb
        (string-append (car lst) " " (helper (cdr lst) adverb))))
  (append (list (string-normalize-spaces (helper adverb-list ""))) (list "д")))

(define (parseLineToList line)
  (let [(line-list (string-split line))]
    (if (and (member? "д" line-list) (> (length line-list) 2))
        (makeLongAdverb line-list)
        line-list)))

(define (readInputFile)
  (define file (open-input-file "input.txt"))
  (define (helper res)
    (let [(line (read-line file))]
      (cond [(equal? line eof) res]
            [else (cons (parseLineToList line) (helper res))])))
  (helper '()))

(define temp (readInputFile))

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

(define (isPlural word)
  (string=? (caddr word) "мн"))

(define (check-noun-verb noun verb)
  (if (or (and (not (isPlural noun)) (not (isPlural verb)))
          (and (isPlural noun) (isPlural verb)))
      #t #f))

(define getRandomSentence-format1
  (lambda ()
    (define noun (getNoun))
    (define adjective (getAdjective))
    (define verb (getVerb))
    (define adverb (getAdverb))

    (if (and (check-adj-noun adjective noun) (check-noun-verb noun verb))
        (apply string-append "" (append (list (string-titlecase (car adjective)) " " (car noun)) (list " " (car verb) " " (car adverb) ".")))
        (getRandomSentence-format1))))

(define getRandomSentence-format2
  (lambda ()
    (define two-nouns (cons (getNoun) (getNoun)))
    (define two-adjectives (cons (getAdjective) (getAdjective)))
    (define verb (getVerb))
    (define adverb (getAdverb))

    (if (and
          (and (not (string=? (car (car two-nouns)) (car (cdr two-nouns))))
               (and (check-adj-noun (car two-adjectives) (car two-nouns)) (check-adj-noun (cdr two-adjectives) (cdr two-nouns))))
          (isPlural verb))
        (apply string-append "" (append
                                 (list (string-titlecase (car (car two-adjectives))) " " (car (car two-nouns)) " и " (car (cdr two-adjectives)) " " (car (cdr two-nouns)))
                                 (list " " (car verb) " " (car adverb) ".")))
        (getRandomSentence-format2))))

(define getRandomSentence
  (lambda ()
    (with-output-to-file
      "result.txt"
      (lambda ()
        (display
          (if (= (random 2) 0)
            (getRandomSentence-format1)
            (getRandomSentence-format2)))
        (newline))
      #:exists `append)))
