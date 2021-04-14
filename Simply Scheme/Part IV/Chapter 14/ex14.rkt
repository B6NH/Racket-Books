; Exercise 14

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (same-shape? a b)
  (if (empty? a)
      (empty? b)
      (and (not (empty? b))
           (= (count (first a))
              (count (first b)))
           (same-shape? (bf a) (bf b)))))



(and
  (equal? (same-shape? '(the fool on the hill)
                       '(you like me too much))
          #t)
  (equal? (same-shape? '(the fool on the hill)
                       '(and your bird can sing))
          #f))
