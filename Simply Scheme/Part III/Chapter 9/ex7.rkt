; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(letterwords ltr snt)
  (keep (lambda(x)(member? ltr x)) snt))
  
  
(equal? (letterwords 'o '(got to get you into my life))
        '(got to you into))
