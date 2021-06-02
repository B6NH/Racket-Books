; Exercise 15

#lang racket

(define (fill-helper vec size index)
  (if (= index -1)
      vec
      (begin
        (vector-set! vec index (make-vector size))
        (fill-helper vec size (- index 1)))))

(define (fill-vector vec size)
  (fill-helper vec size (- (vector-length vec) 1)))

(define (fill-vectors-helper vec lst index)
  (if (= -1 index)
      vec
      (begin
        (helper (vector-ref vec index) lst)
        (fill-vectors-helper vec lst (- index 1)))))

(define (fill-vectors vec lst)
  (fill-vectors-helper vec lst (- (vector-length vec) 1)))

(define (helper vec lst)
  (begin
    (fill-vector vec (car lst))
    (let ((rst (cdr lst)))
      (if (empty? rst)
          vec
          (fill-vectors vec rst)))))

(define (make-array lst)
  (let ((new (make-vector (car lst)))
        (rst (cdr lst)))
    (if (empty? rst)
        new
        (helper new rst))))

(define (array-set! arr indices value)
  (if (empty? (cdr indices))
      (vector-set! arr (car indices) value)
      (array-set! (vector-ref arr (car indices)) (cdr indices) value)))

(define (array-ref arr indices)
  (if (empty? (cdr indices))
      (vector-ref arr (car indices))
      (array-ref (vector-ref arr (car indices)) (cdr indices))))


(define a1 (make-array '(4 5 6)))

(array-set! a1 '(3 2 3) '(the end))
(equal? (array-ref a1 '(3 2 3)) '(the end))

