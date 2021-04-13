; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (addup nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))


(equal? (addup '(1 2 3)) 6)
