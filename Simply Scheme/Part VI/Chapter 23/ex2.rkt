; Exercise 2

#lang racket

(define (helper vec index elm)
  (if (= index -1)
      'done
      (begin (vector-set! vec index elm)
             (helper vec (- index 1) elm))))

(define (my-vfill vec elm)
  (helper vec (- (vector-length vec) 1) elm))

(define vec (vector 'one 'two 'three 'four))
(define d (my-vfill vec 'yeah))


(equal? vec (vector 'yeah 'yeah 'yeah 'yeah))
