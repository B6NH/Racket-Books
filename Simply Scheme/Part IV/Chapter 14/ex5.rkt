; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (letter-count s)
  (if (empty? s)
      0
      (+ (count (first s))
         (letter-count (bf s)))))

(equal? (letter-count '(fixing a hole)) 11)
