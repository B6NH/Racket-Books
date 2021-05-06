; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(equal?
  (cond
    ((= 2 3) (show '(lady madonna)) '(i call your name))
    ((< 2 3) (show '(the night before)) '(hello little girl))
    (else '(p.s. i love you)))
  '(hello little girl))
