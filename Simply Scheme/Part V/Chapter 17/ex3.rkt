; Exercise 3

#lang racket

(define functions
  (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4)))

(and
  (equal? ((car functions) 5) 6)
  (equal? ((cadr functions) 1) 3)
  (equal? ((caddr functions) 6) 9))
