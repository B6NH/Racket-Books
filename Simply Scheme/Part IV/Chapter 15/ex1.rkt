; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (to-binary n)
  (if (zero? n)
      0
      (let ((next (to-binary (quotient n 2))))
        (word (if (zero? next) "" next)
              (remainder n 2)))))

(and
  (equal? (to-binary 0) 0)
  (equal? (to-binary 1) 1)
  (equal? (to-binary 9) 1001)
  (equal? (to-binary 13) 1101)
  (equal? (to-binary 69) 1000101)
  (equal? (to-binary 234) 11101010)
  (equal? (to-binary 542364) 10000100011010011100))
