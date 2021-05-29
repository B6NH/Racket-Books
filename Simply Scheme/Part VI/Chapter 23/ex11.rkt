; Exercise 11

#lang racket

(define *tables* (make-vector 5))

(define *menu* '((potsticker 5.25)
                 (wor-won-ton 7.95)
                 (egg-rolls 2.5)
                 (shin-shin-special-prawns 8)))

(define (price-helper item menu)
  (if (equal? item (caar menu))
      (cadar menu)
      (price-helper item (cdr menu))))

(define (price item)
  (price-helper item *menu*))

(define (order table item)
  (let ((index (- table 1)))
    (vector-set!
      *tables*
      index
      (+ (vector-ref *tables* index)
      (price item)))))

(define (bill table)
  (let ((index (- table 1)))
    (let ((sum (vector-ref *tables* index)))
      (begin
        (vector-set! *tables* index 0)
        sum))))


(order 2 'egg-rolls)
(order 2 'shin-shin-special-prawns)

(order 3 'potsticker)
(order 3 'wor-won-ton)
(order 3 'egg-rolls)
(order 3 'shin-shin-special-prawns)

(order 5 'wor-won-ton)
(order 5 'potsticker)

(and
  (equal? (vector-ref *tables* 0) 0)
  (equal? (vector-ref *tables* 1) 10.5)
  (equal? (vector-ref *tables* 2) 23.7)
  (equal? (vector-ref *tables* 3) 0)
  (equal? (vector-ref *tables* 4) 13.2)
  (equal? (bill 2) 10.5)
  (equal? (bill 5) 13.2)
  (equal? *tables* #(0 0 23.7 0 0))
  (equal? (bill 3) 23.7)
  (equal? *tables* #(0 0 0 0 0)))
