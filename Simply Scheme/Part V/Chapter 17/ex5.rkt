; Exercise 5

#lang racket

(define (max2 a b)
  (if (> b a) b a))

(define (helper lst best)
  (if (empty? lst)
      best
      (helper (cdr lst)
              (max2 (car lst) best))))

(define (mymax number . rest-of-numbers)
  (helper rest-of-numbers number))

(and
  (equal? (mymax 5) 5)
  (equal? (mymax 4 9 1 8) 9))
