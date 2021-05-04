; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (true-for-any-pair? pred snt)
  (and (not (or (empty? snt)
                (empty? (bf snt))))
       (or (pred (first snt) (first (bf snt)))
           (true-for-any-pair? pred (bf snt)))))

(define (true-for-all-pairs? pred snt)
  (not (true-for-any-pair? (lambda (x y) (not (pred x y))) snt)))


(and
  (equal? (true-for-all-pairs? equal? '(a b c c d)) #f)
  (equal? (true-for-all-pairs? equal? '(a a a a a)) #t)
  (equal? (true-for-all-pairs? < '(20 16 5 8 6)) #f)
  (equal? (true-for-all-pairs? < '(3 7 19 22 43)) #t))
