; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(square x)
  (* x x))

(first (square 7))
(first '(square 7))
