; Exercise 9

#lang racket

(define (discount price disc)
  (- price(* price (/ disc 100.))))
  

(discount 10 5)
(discount 29.90 50)
