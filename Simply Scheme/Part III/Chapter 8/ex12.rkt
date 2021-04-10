; Exercise 12

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (count-ums s)
  (count (keep (lambda(x)(equal? x 'um)) s)))


(equal? (count-ums
         '(today um we are going to
           um talk about functional
           um programming))
        3)
