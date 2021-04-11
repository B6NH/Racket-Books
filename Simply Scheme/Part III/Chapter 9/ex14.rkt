; Exercise 14

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(substitute a b s)
  (every
    (lambda(x)
      (if (equal? x b) a x))
    s))


(equal? (substitute 'maybe 'yeah '(she loves you yeah yeah yeah))
        '(she loves you maybe maybe maybe))
