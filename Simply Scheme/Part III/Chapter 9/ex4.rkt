; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (who sent)
  (every (describe sent) '(pete roger john keith)))

(define (describe sent)
  (lambda(person)(se person sent)))


(equal? (who '(sells out))
       '(pete sells out roger sells out
         john sells out keith sells out))
