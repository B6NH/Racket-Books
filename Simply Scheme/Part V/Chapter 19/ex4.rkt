; Exercise 4

#lang racket

(require (planet dyoo/simply-scheme:2:2))

(define (helper comb acc lst)
  (if (empty? lst)
      acc
      (helper comb (comb acc (car lst)) (cdr lst))))

(define (left-accumulate comb lst)
  (helper comb (comb (car lst) (cadr lst))
               (cddr lst)))


(and
  (equal? (accumulate - '(2 3 4 5)) (- 2 (- 3 (- 4 5))))
  (equal? (left-accumulate - '(2 3 4 5)) (- (- (- 2 3) 4) 5)))
