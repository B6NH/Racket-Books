; Exercise 14

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(third w)
  (first(bf (bf w))))

(third 'abxd)
