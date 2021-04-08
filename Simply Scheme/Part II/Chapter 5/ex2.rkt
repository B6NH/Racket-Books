; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(f1 a b)
  (se(bf a)(bl b)))
  
(define(f2 a b)
  (se(bf a)(bl b)(word(first a)(last b))))

(define(f3 a b)
  (se a a))
  
(define(f4 a b)
  (word(second a)(second b)))

  
(f1 '(a b c) '(d e f))
(f2 '(a b c) '(d e f))
(f3 '(a b c) '(d e f))
(f4 '(a b c) '(d e f))
