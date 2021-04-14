; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define (odds s)
  (if (empty? s)
      s
      (se (first s)
          (if (= (count s) 1)
            '()
            (odds (bf (bf s)))))))


(and
  (equal? (odds '(i lost my little girl))
          '(i my girl))
  (equal? (odds '(i lost my little))
          '(i my))
  (equal? (odds '(i lost my))
          '(i my)))
