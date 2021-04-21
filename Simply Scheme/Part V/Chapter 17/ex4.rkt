; Exercise 4

#lang racket


(define (mystery lst)
  (mystery-helper lst '()))

(define (mystery-helper lst other)
  (if (null? lst)
      other
      (mystery-helper (cdr lst) (cons (car lst) other))))

(and
  (equal? (mystery '()) '())
  (equal? (mystery '(1 2 3)) '(3 2 1)))

