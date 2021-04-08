; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(indef-article x)
  (se
    (word 'a (if(member?(first x)'aeiou) 'n ""))
    x))


(indef-article 'beatle)
(indef-article 'album)
(indef-article 'apple)
