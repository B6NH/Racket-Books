; Exercise 13

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(compose f g)
  (lambda(x)(f(g x))))
  
(define second (compose first bf))
  
(and
  (equal? ((compose sqrt abs) -25) 5)
  (equal? (second '(higher order function)) 'order))
