; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

;; Check if sentence matches the pattern
(define (match pattern sent)

  ;; Database is initially empty
  (match-using-known-values pattern sent '()))

;; Match with database values
(define (match-using-known-values pattern sent known-values)
  (cond

    ;; Return database
    ;; If the pattern is empty, the sentence should also be empty
    ((empty? pattern)
     (if (empty? sent) known-values 'failed))

    ;; First element in pattern is special symbol
    ((special? (first pattern))

     ;; Placeholder is special symbol (possibly with name)
     (let ((placeholder (first pattern)))

       ;; Match special symbol
       (match-special (first placeholder) ;; howmany (symbol)
                      (bf placeholder) ;; name
                      (bf pattern) ;; rest of pattern
                      sent ;; sentence
                      known-values))) ;; database

    ;; Pattern is not empty and doesnt contain special
    ;; symbol at the beginning so sentence can't be empty
    ((empty? sent) 'failed)

    ;; Match one normal word and call function recursively
    ((equal? (first pattern) (first sent))
     (match-using-known-values
      (bf pattern) (bf sent) known-values))

    ;; Match failed
    (else 'failed)))

;; Check special symbol
(define (special? wd)
  (member? (first wd) '(* & ? !)))

;; Match special symbol
(define (match-special howmany name pattern-rest sent known-values)

  (if (and (not (empty? name))
           (number? (first name)))

      ;; Placeholder of the form *15x
      (match-n-words name pattern-rest sent known-values)

      ;; Get placeholder value
      (let ((old-value (lookup name known-values)))
        (cond

          ;; Value already known
          ((not (equal? old-value 'no-value))

          ;; Check if old-value size is consistent with howmany size
          (if (length-ok? old-value howmany)

            ;; Match with known value
            (already-known-match
              old-value pattern-rest sent known-values)

            ;; Fail
            'failed))

          ;; Match all symbols using the same function with different parameters
          ((equal? howmany '?)

            ;; No min, max
            (longest-match name pattern-rest sent 0 #t known-values))

          ((equal? howmany '!)

            ;; Min, max
            (longest-match name pattern-rest sent 1 #t known-values))

          ((equal? howmany '*)

            ;; No min, no max
            (longest-match name pattern-rest sent 0 #f known-values))

          ((equal? howmany '&)

            ;; Min, no max
            (longest-match name pattern-rest sent 1 #f known-values))))))

;; Check pattern size consistency
(define (length-ok? value howmany)
  (cond

    ;; If old value is empty, placeholder with the same name should be '?' or '*'
    ((empty? value) (member? howmany '(? *)))

    ;; If old value is longer than 1 word it must be '*' or '&'
    ((not (empty? (bf value))) (member? howmany '(* &)))

    ;; One word can match anything
    (else #t)))

(define (already-known-match value pattern-rest sent known-values)

  ;; Remove matching part of sentence
  (let ((unmatched (chop-leading-substring value sent)))

    ;; Check match
    (if (not (equal? unmatched 'failed))

        ;; Continue after successful match
        (match-using-known-values
          pattern-rest unmatched known-values)

        ;; Return 'failed
        'failed)))

;; Remove value from sentence
(define (chop-leading-substring value sent)
  (cond

    ;; Value already removed
    ((empty? value) sent)

    ;; Sentence can't be empty if there is value to remove
    ((empty? sent) 'failed)

    ;; Remove 1 word and continue recursively
    ((equal? (first value) (first sent))
     (chop-leading-substring (bf value) (bf sent)))

    ;; Value doesn't match sentence
    (else 'failed)))

;; Find longest match with unknown name
(define (longest-match name pattern-rest sent min max-one? known-values)
  (cond

    ;; Empty sentence
    ((empty? sent)

     ;; Min is zero for '?' and '*'
     (if (= min 0)

         ;; Add placeholder name with empty
         ;; value to database and continue
         (match-using-known-values
              pattern-rest
              sent
              (add name '() known-values))

         ;; Empty sentence doesn't match '!' and '&' (min = 1)
         'failed))

    ;; Match at most 1 word ('?' and '!')
    (max-one?

      ;; Try matching 1 word
      (lm-helper
        name
        pattern-rest

        ;; Start with one word
        (se (first sent)) ;; one matched word (sentence is not empty, look above!)
        (bf sent) ;; unmatched words

        min
        known-values))

    ;; Match symbols with no size limit ('*' and '&')
    (else

      ;; Start by trying to match whole sentence
      (lm-helper
        name
        pattern-rest

        ;; Start with whole sentence
        sent ;; all words matched
        '() ;; no remaining words

        min
        known-values))))

(define (lm-helper name pattern-rest sent-matched sent-unmatched min known-values)

  ;; Check minimum word length (can sentence-matched be empty at all?)
  (if (< (length sent-matched) min)
      'failed

      ;; Match using known values with unmatched part of sentence
      ;; Matched part of sentence is added to dictionary
      (let ((tentative-result (match-using-known-values
                                 pattern-rest
                                 sent-unmatched
                                 (add name sent-matched known-values))))
        (cond

          ;; Return successfull result
          ((not (equal? tentative-result 'failed)) tentative-result)

          ((empty? sent-matched) 'failed)

          ;; Try different match
          (else
            (lm-helper
              name
              pattern-rest

              ;; Shorten matched part of the sentence
              (bl sent-matched)

              ;; Extend unmatched part of the sentence
              (se (last sent-matched) sent-unmatched)

              min
              known-values))))))

;;; Known values database abstract data type

;; Get value from database
(define (lookup name known-values)
  (cond
    ((empty? known-values) 'no-value)
    ((equal? (first known-values) name)
     (get-value (bf known-values)))
    (else (lookup name (skip-value known-values)))))

;; Get value from the dictionary up to the symbol '!'
(define (get-value stuff)
  (if (equal? (first stuff) '!)
      '()
      (se (first stuff) (get-value (bf stuff)))))

;; Skip value from the dictionary up to the symbol '!'
(define (skip-value stuff)
  (if (equal? (first stuff) '!)
      (bf stuff)
      (skip-value (bf stuff))))

;; Add value to dictionary
(define (add name value known-values)
  (if (empty? name)
      known-values
      (se known-values name value '!)))

; ------------------------------------------------------

; Exercise functions

(define (empty-or-not-number p)
  (or (empty? p) (not (number? (first p)))))

(define (get-number p)
  (if (empty-or-not-number p)
      ""
      (word (first p) (get-number (bf p)))))

(define (skip-number p)
  (if (empty-or-not-number p)
    p
    (skip-number (bf p))))

(define (match-n-words name pattern-rest sent known-values)
  (let ((num (get-number name)))
    (if (>= (length sent) num)
        (match-using-known-values
          pattern-rest
          (drop sent num)
          (add (skip-number name) (take sent num) known-values))
        'failed)))

; ------------------------------------------------------

;; Exercises about Using the Pattern Matcher

(displayln "***Exercises***")

; Ex 1

(define 3c '(* c * c * c *))

(and
  (equal? (match 3c '(love me do)) 'failed)
  (equal? (match 3c '(love c do)) 'failed)
  (equal? (match 3c '(c love c me do c)) '())
  (equal? (match 3c '(bunny c love c me do c hey c can)) '()))

; Ex 2

(define 2copies '(*copy *copy))

(and
  (equal? (match 2copies '(a b a b)) '(copy a b !))
  (equal? (match 2copies '(a b c a b c)) '(copy a b c !))
  (equal? (match 2copies '(a b a b a)) 'failed)
  (equal? (match 2copies '(b a a b)) 'failed))

; Ex 3

(define max3 '(? ? ?))

(and
  (equal? (match max3 '(yes)) '())
  (equal? (match max3 '(these characters)) '())
  (equal? (match max3 '()) '())
  (equal? (match max3 '(if the match)) '())
  (equal? (match max3 '(the question mark means)) 'failed))

; Ex 4

(define min3 '(! ! ! *))

(and
  (equal? (match min3 '()) 'failed)
  (equal? (match min3 '(love)) 'failed)
  (equal? (match min3 '(this case)) 'failed)
  (equal? (match min3 '(named special words)) '())
  (equal? (match min3 '(if the same placeholder)) '()))

; Ex 5

(define p1 '(*x *y *y *x))

(and
  (equal? (match p1 '(len len)) '(x len ! y !))
  (equal? (match p1 '(bird fish fish bird)) '(x bird ! y fish !)))

; Ex 6

(define p2 '(*x *y &y &x))

(and
  (equal? (match p2 '(fire water water fire))
          '(x fire ! y water !))
  (equal? (match p2 '(al al al al))
          '(x al ! y al !)))

; Ex 7

(define p3 '(*x *y &y &x))

(and
  (equal? (match p3 '(a b a b a a)) '(x a ! y b a !))
  (equal? (match p3 '(a b a a a b)) '(x a b ! y a !)))

; ------------------------------------------------------

;; Tests

(displayln "***Tests***")

(define db1 '(x mega drive ! y water !))
(define many '(liquid drum mix))
(define single '(drugs))

(and

  ;; Lookup
  (equal? (lookup 'x db1) '(mega drive))
  (equal? (lookup 'y db1) '(water))
  (equal? (lookup 'z db1) 'no-value)

  ;; Get & Skip
  (equal? (get-value db1) '(x mega drive))
  (equal? (skip-value db1) '(y water !))

  ;; Add
  (equal? (add 'neuroframe '(the wrath of code) db1)
         '(x mega drive ! y water ! neuroframe the wrath of code !))

  ; Get number of words
  (equal? (get-number '3front) 3)
  (equal? (get-number '25back) 25)
  (equal? (get-number '9) 9)

  ; Skip number part of placeholder
  (equal? (skip-number '5canary) 'canary)
  (equal? (skip-number '123crocodile) 'crocodile)
  (equal? (skip-number '515) "")

  ; Match n words
  (equal? (match '(*3front *back) '(your mother should know))
          '(front your mother should ! back know !)))
