; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (let-it-be sent)
  (accumulate (lambda (x y) y) sent))
  
  
(equal? (let-it-be '(jeden dwa trzy cztery)) 'cztery)
