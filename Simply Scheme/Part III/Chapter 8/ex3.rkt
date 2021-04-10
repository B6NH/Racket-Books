; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(double x)
  (* x 2))

(define (f a)
  (keep even? a))

(define (g b)
  (every b '(blue jay way)))

(define (h c d)
  (c (c d)))

(define (i e)
  (/ (accumulate + e) (count e)))
  
(and
  (equal? (f '(1 2 3 4 5 6 7)) '(2 4 6))
  (equal? (g (lambda(x)(word x 'es))) '(bluees jayes wayes))
  (equal? (h (lambda(x)(+ x 2))6) 10)
  (equal? (i '(24. 55 17 87 100)) 56.6)
  (equal?(accumulate - '(4 3 2 1))(- 4(- 3(- 2 1))))
  (equal?(sqrt 9)3)
  (equal?((repeated double 2)2)8)
  (equal?((repeated sqrt 3)65536)4)
  (equal?((repeated first 2)'(army of the night))'a)
  (equal?((repeated (repeated bf 3) 2)'amaranthe)'the))

  
; error
; (repeated even? 2)
