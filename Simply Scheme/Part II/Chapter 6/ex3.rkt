; Exercise 3

#lang racket

(define (sign number)
  (if (< number 0)
      'negative
      (if (= number 0)
    'zero
    'positive)))
    
(define (sign2 number)
  (cond
    ((= number 0)'zero)
    ((< number 0)'negative)
    (else 'positive)))
    
(sign -2)
(sign 0)
(sign 4)

(sign2 -2)
(sign2 0)
(sign2 4)
