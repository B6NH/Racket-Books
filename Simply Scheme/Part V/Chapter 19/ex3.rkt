; Exercise 3

#lang racket

(define (three-arg-accumulate comb base lst)
  (if (empty? lst)
      base
      (comb (car lst) (three-arg-accumulate comb base (cdr lst)))))


(and
  (equal? (three-arg-accumulate + 0 '(4 5 6)) 15)
  (equal? (three-arg-accumulate + 0 '()) 0)
  (equal? (three-arg-accumulate cons '() '(a b c d e)) '(a b c d e))
  (equal? (three-arg-accumulate - 0 '(8 3 1)) (- 8 (- 3 (- 1 0)))))


