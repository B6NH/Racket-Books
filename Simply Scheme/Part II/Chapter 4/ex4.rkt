; Exercise 4

#lang racket


(define (sphere-volume r)
  (* (/ 4 3) 3.141592654 (* r r r)))

(define (next x)
  (+ x 1))

(define (square x)
  (* x x))
  
(define (triangle-area base height)
  (* 0.5 base height))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sphere-volume 5)
(next 6)
(square 8)
(triangle-area 12 5)
(sum-of-squares 7 9)
