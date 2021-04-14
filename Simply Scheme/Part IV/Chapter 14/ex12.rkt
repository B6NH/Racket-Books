; Exercise 12

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (mysq x)
  (* x x))

(define (progressive-squares? s)
  (or (<= (count s) 1)
      (and (= (mysq (first s))
              (first (bf s)))
           (progressive-squares? (bf s)))))

(and
  (equal? (progressive-squares? '(3 9 81 6561))
          #t)
  (equal? (progressive-squares? '(25 36 49 64))
          #f))
