; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (differences s)
  (if (< (count s) 2)
      '()
      (let ((rst (bf s)))
        (se (- (first rst)
              (first s))
            (differences rst)))))


(equal? (differences '(4 23 9 87 6 12))
        '(19 -14 78 -81 6))
