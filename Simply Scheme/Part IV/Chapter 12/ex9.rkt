; Exercise 9

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define(real-words s)
  (cond
    ((empty? s)'())
    ((real-word?(first s))
     (se (first s)
         (real-words(bf s))))
    (else (real-words(bf s)))))


(equal? (real-words '(the something abc of america the))
        '(something abc america))

