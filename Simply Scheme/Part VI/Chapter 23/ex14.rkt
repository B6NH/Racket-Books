; Exercise 14

#lang racket

(define (fill-helper vec size index)
  (if (= index -1)
      vec
      (begin
        (vector-set! vec index (make-vector size))
        (fill-helper vec size (- index 1)))))

(define (fill-vector vec size)
  (fill-helper vec size (- (vector-length vec) 1)))

(define (matrix-ref m r c)
  (vector-ref (vector-ref m r) c))

(define (matrix-set! m r c value)
  (vector-set! (vector-ref m r) c value))

(define (make-matrix r c)
  (fill-vector (make-vector r) c))


(define mat (make-matrix 3 4))

(matrix-set! mat 0 0 7)
(matrix-set! mat 1 1 6)
(matrix-set! mat 2 2 1)

(and
  (equal? (matrix-ref mat 0 0) 7)
  (equal? (matrix-ref mat 0 1) 0)
  (equal? (matrix-ref mat 0 2) 0)
  (equal? (matrix-ref mat 0 3) 0)
  (equal? (matrix-ref mat 1 0) 0)
  (equal? (matrix-ref mat 1 1) 6)
  (equal? (matrix-ref mat 1 2) 0)
  (equal? (matrix-ref mat 1 3) 0)
  (equal? (matrix-ref mat 2 0) 0)
  (equal? (matrix-ref mat 2 1) 0)
  (equal? (matrix-ref mat 2 2) 1)
  (equal? (matrix-ref mat 2 3) 0))
