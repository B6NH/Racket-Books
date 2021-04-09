; Exercise 10

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(sort2 s)
  (let((fs(first s))(ls(last s)))
    (if(<= fs ls)s(se ls fs))))


(and
  (equal?(sort2 '(5 7))'(5 7))
  (equal?(sort2 '(7 5))'(5 7))
  (equal?(sort2 '(5 5))'(5 5)))
     
