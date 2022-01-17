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

    ;; If the pattern is empty, the sentence should also be empty
    ((empty? pattern)
     (if (empty? sent) known-values 'failed))
    ((special? (first pattern))
     (let ((placeholder (first pattern)))
       (match-special (first placeholder)
                      (bf placeholder)
                      (bf pattern)
                      sent
                      known-values)))
    ((empty? sent) 'failed)
    ((equal? (first pattern) (first sent))
     (match-using-known-values
      (bf pattern) (bf sent) known-values))
    (else 'failed)))

;; Check special symbol
(define (special? wd)
  (member? (first wd) '(* & ? !)))

(define (match-special howmany name pattern-rest sent known-values)
  (let ((old-value (lookup name known-values)))
    (cond
      ((not (equal? old-value 'no-value))
       (if (length-ok? old-value howmany)
        (already-known-match
          old-value pattern-rest sent known-values)
        'failed))
      ((equal? howmany '?)
        (longest-match name pattern-rest sent 0 #t known-values))
      ((equal? howmany '!)
        (longest-match name pattern-rest sent 1 #t known-values))
      ((equal? howmany '*)
        (longest-match name pattern-rest sent 0 #f known-values))
      ((equal? howmany '&)
        (longest-match name pattern-rest sent 1 #f known-values)))))

(define (length-ok? value howmany)
  (cond
    ((empty? value) (member? howmany '(? *)))
    ((not (empty? (bf value))) (member? howmany '(* &)))
    (else #t)))

(define (already-known-match value pattern-rest sent known-values)
  (let ((unmatched (chop-leading-substring value sent)))
    (if (not (equal? unmatched 'failed))
        (match-using-known-values
          pattern-rest unmatched known-values)
        'failed)))

(define (chop-leading-substring value sent)
  (cond
    ((empty? value) sent)
    ((empty? sent) 'failed)
    ((equal? (first value) (first sent))
     (chop-leading-substring (bf value) (bf sent)))
    (else 'failed)))

(define (longest-match name pattern-rest sent min max-one? known-values)
  (cond
    ((empty? sent)
     (if (= min 0)
         (match-using-known-values
              pattern-rest
              sent
              (add name '() known-values))
         'failed))
    (max-one?
      (lm-helper name pattern-rest (se (first sent))
      (bf sent) min known-values))
    (else
      (lm-helper name pattern-rest sent '() min known-values))))

(define (lm-helper name pattern-rest
      sent-matched sent-unmatched min known-values)
  (if (< (length sent-matched) min)
      'failed
      (let ((tentative-result (match-using-known-values
            pattern-rest
            sent-unmatched
            (add name sent-matched known-values))))
(cond ((not (equal? tentative-result 'failed)) tentative-result)
      ((empty? sent-matched) 'failed)
      (else (lm-helper name
            pattern-rest
            (bl sent-matched)
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
         '(x mega drive ! y water ! neuroframe the wrath of code !)))

