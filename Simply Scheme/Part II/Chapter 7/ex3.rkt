; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (superlative adjective wd)
  (se (word adjective 'est) wd))


(superlative 'dumb 'exercise)
