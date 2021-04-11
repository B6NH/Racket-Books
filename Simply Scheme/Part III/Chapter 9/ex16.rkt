; Exercise 16

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(aplize fn)
  (lambda(x)
    (if(number? x)
       (fn x)
       (every fn x))))


(define apl-sqrt (aplize sqrt))

(and
  (equal? (apl-sqrt 36) 6)
  (equal? (apl-sqrt '(1 100 25 16)) '(1 10 5 4)))
