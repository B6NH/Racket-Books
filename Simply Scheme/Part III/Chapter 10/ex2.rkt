; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define (tie-game? position)
  (not (member? '_ position)))

(and
  (equal? (tie-game? 'o__xx____) #f)
  (equal? (tie-game? 'oxoxxoxox) #t))
