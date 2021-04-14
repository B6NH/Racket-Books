; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (remove-once w s)
    (if (empty? s)
        s
        (let ((fst (first s)) (rst (bf s)))
          (if (equal? fst w)
              rst
              (se fst (remove-once w rst))))))


(equal? (remove-once 'morning '(good morning good morning))
        '(good good morning))

