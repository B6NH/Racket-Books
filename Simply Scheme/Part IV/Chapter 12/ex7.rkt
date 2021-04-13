; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (spell-digit digit)
  (item (+ 1 digit)
  '(zero one two three four five six seven eight nine)))

(define(spell-number s)
  (if (empty? s)
      '()
      (se(spell-digit(first s))
         (spell-number(bf s)))))


(equal? (spell-number 1971) '(one nine seven one))
