; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define (helper wd)
  (if (empty? wd)
      '()
      (se wd (helper (bl wd)))))

(define (substrings wd)
  (if (empty? wd)
      '()
      (se (helper wd)
          (substrings (bf wd)))))


(and
  (equal? (substrings "") '())
  (equal? (substrings 'a) '(a))
  (equal? (substrings 'ab) '(ab a b))
  (equal? (helper 'abcd) '(abcd abc ab a))
  (equal? (helper 'bcd) '(bcd bc b))
  (equal? (helper 'cd) '(cd c))
  (equal? (helper 'd) '(d))
  (equal? (substrings 'abcd)
          '(abcd abc ab a
            bcd bc b
            cd c
            d)))

