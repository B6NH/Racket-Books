; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(count-ums s)
  (cond
    ((empty? s) 0)
    ((equal? (first s) 'um )(+ 1(count-ums(bf s))))
    (else(count-ums(bf s)))))


(equal?
  (count-ums
   '(today um we are going to um talk about the combining um method))
  3)
