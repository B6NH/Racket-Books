; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(vowel? letter)
  (member? letter '(a e i o u)))
  
(define(ends-vowel? wd)
  (vowel? (last wd)))
  
(define(even-count? wd)
  (even? (count wd)))

(define(choose-beatles f)
  (keep f '(john paul george ringo)))

(and
  (equal? (choose-beatles ends-vowel?) '(george ringo))
  (equal? (choose-beatles even-count?) '(john paul george)))
