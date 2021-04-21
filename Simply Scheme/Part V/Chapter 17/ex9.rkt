; Exercise 9

#lang racket

(define (mylistref lst n)
  (cond
    ((empty? lst) #f)
    ((zero? n) (car lst))
    (else (mylistref (cdr lst)
                 (- n 1)))))


(and
  (equal? (mylistref '(12 44 55 66) 0) 12)
  (equal? (mylistref '(17 21 25 99) 2) 25)
  (equal? (mylistref '(2 3) 6) #f))
