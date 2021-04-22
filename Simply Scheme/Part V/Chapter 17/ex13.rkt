; Exercise 13

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (deep-count lst)
  (cond
    ((null? lst) 0)
    ((word? lst) 1)
    (else (+ (deep-count (car lst))
             (deep-count (cdr lst))))))


(equal? (deep-count '(((a b) c (d e)) (f g) ((((h))) (i j) k)))
        11)
