; Exercise 10

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(appearances e lst)
  (accumulate +
    (every (lambda(x)(if(equal? e x) 1 0)) lst)))
  
  
(and
  (equal?
    (appearances 'goat '(plan goat singer cream goat disk goat goat))
    4)
  (equal?
    (appearances 'spell '(lemon ground source soap notebook))
    0))
