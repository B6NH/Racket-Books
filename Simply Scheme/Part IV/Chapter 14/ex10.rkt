; Exercise 10

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (count-adjacent-duplicates s)
  (if (<= (count s) 1)
      0
      (let ((rst (bf s)))
        (+ (if (equal? (first s)(first rst)) 1 0)
          (count-adjacent-duplicates rst)))))

(and
  (equal? (count-adjacent-duplicates '(y a b b a d a b b a d o o))
          3)
  (equal? (count-adjacent-duplicates '(yeah yeah yeah))
          2))
