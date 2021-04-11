; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define second
  (lambda(stuff)(first(bf stuff))))
  
(define make-adder
  (lambda(num)(lambda(x)(+ num x))))


(and
  (equal? (second 'abc) 'b)
  (equal? ((make-adder 6)3) 9))
