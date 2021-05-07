; Exercise 2

#lang racket

(define rvalue (newline))

(and
  (not (number? rvalue))
  (not (list? rvalue))
  (void? rvalue))
