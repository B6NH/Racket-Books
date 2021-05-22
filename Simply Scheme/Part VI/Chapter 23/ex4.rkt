; Exercise 4

#lang racket

(define (helper vec acc len)
  (if (= len -1)
      acc
      (helper vec (cons (vector-ref vec len) acc) (- len 1))))

(define (vec-to-l vec)
  (helper vec '() (- (vector-length vec) 1)))


(and
  (equal? (vec-to-l #()) '())
  (equal? (vec-to-l (vector 2 6 4 8 10)) '(2 6 4 8 10)))
