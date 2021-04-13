; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(initials s)
  (if (empty? s)
      '()
      (se (first(first s))
          (initials (bf s)))))


(equal? (initials '(if i needed someone)) '(i i n s))
