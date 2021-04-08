; Exercise 16

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(two-first a b)
  (word(first a)(first b)))
  
(define(two-first-sent s)
  (word(first(first s))
       (first(last s))))
  
(two-first 'brian 'harvey)
(two-first-sent '(brian harvey))
