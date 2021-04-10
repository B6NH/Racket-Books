; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(letter-count s)
  (accumulate + (every count s)))


(equal?(letter-count '(fixing a hole))11)
