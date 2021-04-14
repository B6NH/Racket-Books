; Exercise 13

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (is-vowel? x)
  (member? x 'aeiou))

(define (has-vowel? w)
  (not (empty? (keep is-vowel? w))))

(define (adday x)
  (word x 'ay))

(define (pigl wd)
  ((if (has-vowel? wd)
       helper
       adday)
    wd))

(define (helper wd)
  (let ((fst (first wd)))
    (if (is-vowel? fst)
        (adday wd)
        (helper (word (bf wd) fst)))))

(and
  (equal? (pigl 'spaghetti) 'aghettispay)
  (equal? (pigl 'angel) 'angelay)
  (equal? (pigl 'frzzmlpt) 'frzzmlptay))
