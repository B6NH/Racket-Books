; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (downup4 wd)
  (se wd (bf wd) (bf(bf wd)) (bf(bf(bf wd))) (bf(bf wd))  (bf wd) wd))


(equal? (downup4 'paul) '(paul aul ul l ul aul paul))
