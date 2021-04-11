; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(prepend-every s lst)
  (every (lambda(x)(word s x)) lst))

(and
  (equal? (prepend-every 's '(he aid he aid))
          '(she said she said))
  (equal? (prepend-every 'anti '(dote pasto gone body))
          '(antidote antipasto antigone antibody)))
