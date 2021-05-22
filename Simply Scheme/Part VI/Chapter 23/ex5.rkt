; Exercise 5

#lang racket

(define (double x)
  (* x 2))

(define (helper fn new old index)
  (if (= index -1)
      new
      (begin
        (vector-set! new index (fn (vector-ref old index)))
        (helper fn new old (- index 1)))))

(define (vec-map fn vec)
  (let ((len (vector-length vec)))
    (let ((new (make-vector len)))
        (helper fn new vec (- len 1)))))


(define vector_a #(5 23 4 5 8))

(and
  (equal? (vec-map double vector_a) #(10 46 8 10 16))
  (equal? vector_a #(5 23 4 5 8)))
