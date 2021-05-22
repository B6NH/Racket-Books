; Exercise 1

#lang racket

(define (helper vec index acc)
  (if (= index -1)
      acc
      (helper vec (- index 1) (+ (vector-ref vec index) acc))))

(define (sum-vector vec)
  (helper vec (- (vector-length vec) 1) 0))


(and
  (equal? (sum-vector '#(6 7 8)) 21)
  (equal? (sum-vector '#(-5 2 20)) 17)
  (equal? (sum-vector '#(82 6 15)) 103))
