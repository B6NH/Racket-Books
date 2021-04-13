; Exercise 10

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(remove w s)
  (cond
    ((empty? s)'())
    ((equal? w (first s))
     (remove w (bf s)))
    (else (se (first s)
              (remove w (bf s))))))

(equal? (remove 'the '(the song love of the loved by the beatles))
        '(song love of loved by beatles))
