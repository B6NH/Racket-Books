; Exercise 10

#lang racket

(define (tip bill)
  (- (ceiling (+ bill (* 0.15 bill))) bill))
  
(tip 19.98)
(tip 29.23)
(tip 7.54)
