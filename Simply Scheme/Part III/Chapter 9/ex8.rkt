; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (hang-letter letter guesses)
  (if (member? letter guesses)
      letter
      '_))

(define(hang secret guesses)
  (accumulate word
    (every (lambda(l)(hang-letter l guesses)) secret)))


(equal? (hang 'potsticker 'etaoi) '_ot_ti__e_)
