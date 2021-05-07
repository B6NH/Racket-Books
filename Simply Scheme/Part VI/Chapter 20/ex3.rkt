; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (myshow stuff)
  (begin
    (display stuff)
    (newline)))


(and
  (void? (show '(something to show)))
  (void? (show 23))
  (void? (myshow '(something to show)))
  (void? (myshow 23)))
