; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(member2 v s)
  (cond
    ((empty? s) #f)
    ((equal? (first s) v) #t)
    (else (member2 v (bf s)))))

(and
  (equal? (member2 5 '(12 33 5 7)) #t)
  (equal? (member2 11 '(12 49 10 1)) #f))
