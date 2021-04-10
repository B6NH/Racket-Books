; Exercise 9

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define pro '(what procedure can you use))
(define radio '(over a noisy radio connection))
(define wind '(her hair was windswept))


(and
  (equal? (every (lambda(x) x) pro) pro)
  (equal? (keep (lambda(x) #t) radio) radio)
  (equal? (accumulate (lambda(x y)(se x y)) wind) wind))
