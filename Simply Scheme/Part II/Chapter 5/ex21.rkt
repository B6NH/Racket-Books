; Exercise 21

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(query s)
  (se (first(bf s))
      (first s)
      (bf (bf (bl s)))
      (word (last s) '?)))

(query '(you are experienced))
(query '(i should have known better))
