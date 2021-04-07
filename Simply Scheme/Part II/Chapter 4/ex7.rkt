; Exercise 7

#lang racket

(define (square x)
  (* x x))

(define (absv x)
  (sqrt(square x)))
  
(absv 0)
(absv 12)
(absv -25)
