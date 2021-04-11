; Exercise 9

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(common-words a b)
  (keep (lambda(x)(member? x b)) a))
  

(equal? (common-words '(alive pepper lead goat lawyer sheep)
                      '(winter sheep red alarm goat pepper))
        '(pepper goat sheep))
