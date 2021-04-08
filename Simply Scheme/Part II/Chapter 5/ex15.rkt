; Exercise 15

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(first-two w)
  (word(first w)(first(bf w))))
  
(first-two 'ambulatory)
