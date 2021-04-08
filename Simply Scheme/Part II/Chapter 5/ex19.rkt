; Exercise 19

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(insert-and s)
  (se (bl s) 'and (last s)))
  
(insert-and '(john bill wayne fred joey))
