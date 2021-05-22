; Exercise 6

#lang racket

(define (double x)
  (* x 2))

(define (helper fn vec index)
  (if (= index -1)
      vec
      (begin
        (vector-set! vec index (fn (vector-ref vec index)))
        (helper fn vec (- index 1)))))

(define (vec-map fn vec)
  (helper fn vec (- (vector-length vec) 1)))


(define vector_a (vector 5 23 4 5 8))

(and
  (equal? (vec-map double vector_a) #(10 46 8 10 16))
  (equal? vector_a #(10 46 8 10 16)))
