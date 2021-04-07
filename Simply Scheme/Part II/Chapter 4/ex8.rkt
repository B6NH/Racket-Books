; Exercise 8

#lang racket

(define (pow x y)
  (cond
    ((= y 0) 1)
    ((> y 0)(* x (pow x (- y 1))))
    (else (pow (/ 1 x) (- y)))))

  
(define (scientific a b)
  (* a (pow 10 b)))
  

(scientific 5 0)
(scientific 6 1)
(scientific 7 3)
(scientific 42 -5)
