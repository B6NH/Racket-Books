; Exercise 6

#lang racket

(define (square x)
  (* x x))

(define (fourth x)
  (* x x x x))
  
(define (fourth2 x)
  (square (square x)))
  
(fourth 4)
(fourth2 4)
