; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(change w)
  (cond
    ((number? w)(* w 2))
    ((equal? w 'good)'great)
    ((equal? w 'bad)'terrible)
    ((equal? w 'good)'great)
    ((equal? w 'like)'love)
    (else w)))
    

(define(exaggerate s)
  (every change s))
  
(and
  (equal? (exaggerate '(i ate 3 potstickers))
                      '(i ate 6 potstickers))
  (equal? (exaggerate '(the chow fun is good here))
                      '(the chow fun is great here))
  (equal? (exaggerate '(she had a bad day))
                      '(she had a terrible day))
  (equal? (exaggerate '(i like her))
                      '(i love her)))
