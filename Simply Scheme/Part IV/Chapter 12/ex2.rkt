; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (acronym sent)
  (if (= (count sent) 1)
      (first(first sent))
      (word (first (first sent))
      (acronym (bf sent)))))


(equal? (acronym '(here is a definition))
        'hiad)


