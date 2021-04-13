; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(copies n w)
  (if (= n 0)
      '()
      (se w (copies (- n 1) w))))

(equal? (copies 8 'spam)
        '(spam spam spam spam spam spam spam spam))
