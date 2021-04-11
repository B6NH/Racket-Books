; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(square x)
  (* x x))

(define(sentence-version f)
  (lambda(s)(every f s)))

(and
  (equal? ((sentence-version first) '(if i fell)) '(i i f))
  (equal?((sentence-version square) '(8 2 4 6)) '(64 4 16 36)))
