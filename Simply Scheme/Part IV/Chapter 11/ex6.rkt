; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(countdown n)
  (if (= n 0)
      '(blastoff!)
      (se n (countdown(- n 1)))))


(and
  (equal? (countdown 10) '(10 9 8 7 6 5 4 3 2 1 blastoff!))
  (equal? (countdown 3) '(3 2 1 blastoff!)))
