; Exercise 12

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(first-last s)
  (keep (lambda(x)
          (equal? (first x) (last x)))
        s))


(equal?
  (first-last '(california ohio nebraska alabama alaska massachusetts))
  '(ohio alabama alaska))
