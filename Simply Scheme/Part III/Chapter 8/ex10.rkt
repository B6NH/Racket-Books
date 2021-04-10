; Exercise 10

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(true-for-all? fn lst)
  (= (count(keep fn lst))
     (count lst)))


(and
  (equal? (true-for-all? even? '(2 4 6 8)) #t)
  (equal? (true-for-all? even? '(2 6 3 4)) #f))
