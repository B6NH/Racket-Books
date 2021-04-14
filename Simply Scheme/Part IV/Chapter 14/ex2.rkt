; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(up w)
  (if (empty? w)
      '()
      (se (up (bl w)) w)))

(equal? (up 'town) '(t to tow town))
