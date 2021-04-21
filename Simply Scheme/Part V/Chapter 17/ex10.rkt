; Exercise 10

#lang racket

(define (mylength lst)
  (if (empty? lst)
      0
      (+ 1 (mylength (cdr lst)))))


(and
  (equal? (mylength '()) 0)
  (equal? (mylength '(5)) 1)
  (equal? (mylength '(7 12 8 9 25)) 5))
