; Exercise 5

#lang racket

(define(fa-to-ce temperature)
  (*(/ 5 9)(- temperature 32.)))
  
(define(ce-to-fa temperature)
  (+(*(/ 9 5)temperature)32))
  
(fa-to-ce 68)
(ce-to-fa 20)
