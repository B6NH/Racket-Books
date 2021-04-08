; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(type-of x)
  (cond
    ((number? x) 'number)
    ((word? x) 'word)
    ((sentence? x) 'sentence)
    ((boolean? x) 'boolean)))
    
    
(type-of '(getting better))
(type-of 'revolution)
(type-of (= 3 3))
(type-of (> 2 3))
(type-of 25)
