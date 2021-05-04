; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (true-for-any-pair? pred snt)
  (and (not (or (empty? snt)
                (empty? (bf snt))))
       (or (pred (first snt) (first (bf snt)))
           (true-for-any-pair? pred (bf snt)))))


(and
  (equal? (true-for-any-pair? > '()) #f)
  (equal? (true-for-any-pair? < '(12)) #f)
  (equal? (true-for-any-pair? equal? '(a b c b a)) #f)
  (equal? (true-for-any-pair? equal? '(a b c c d)) #t)
  (equal? (true-for-any-pair? < '(20 16 5 8 6)) #t))



