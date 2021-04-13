; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(numbers x)
  (cond
    ((empty? x)'())
    ((number? (first x))
     (se (first x) (numbers (bf x))))
    (else (numbers (bf x)))))

(equal? (numbers '(76 trombones and 110 cornets))
        '(76 110))
