; Exercise 11

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(count s)
  (if(empty? s)
     0
     (+ 1(count(bf s)))))

(and
  (equal? (count 'love) 4)
  (equal? (count '(song time scheme)) 3))

