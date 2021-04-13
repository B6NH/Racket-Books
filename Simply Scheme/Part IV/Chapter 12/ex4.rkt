; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(fn s)
  (if(empty? s)
     s
     (se (fn(bf s))
         (first s))))

(fn '(fix the bug in the))
