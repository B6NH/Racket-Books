; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (true-for-all-pairs? pred lst)
  (or (empty? lst)
      (empty? (bf lst))
      (and (pred (first lst) (first (bf lst)))
           (true-for-all-pairs? pred (bf lst)))))

(and
  (equal? (true-for-all-pairs? equal? '(a b c c d)) #f)
  (equal? (true-for-all-pairs? equal? '(a a a a a)) #t)
  (equal? (true-for-all-pairs? < '(20 16 5 8 6)) #f)
  (equal? (true-for-all-pairs? < '(3 7 19 22 43)) #t))
