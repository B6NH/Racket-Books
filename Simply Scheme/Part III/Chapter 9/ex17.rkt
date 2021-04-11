; Exercise 17

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(mykeep pred lst)
  (accumulate se
    (every (lambda(x)
             (if (pred x) x '())) lst)))


(define answer '(2 4 6 8 10 20))

(equal? (mykeep even? '(1 2 3 4 5 6 7 8 9 10 11 13 15 17 20 23 31))
         answer)
