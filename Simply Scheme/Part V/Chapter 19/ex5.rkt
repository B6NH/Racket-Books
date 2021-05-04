; Exercise 5

#lang racket

(define (true-for-all? pred lst)
  (or (empty? lst)
      (and (pred (car lst))
           (true-for-all? pred (cdr lst)))))

(and
  (equal? (true-for-all? even? '(2 4 6 8)) #t)
  (equal? (true-for-all? even? '(2 6 3 4)) #f))
