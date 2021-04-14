; Exercise 12

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(second s)
  (first(bf s)))

(define (roman-value letter)
  (cond ((equal? letter 'i) 1)
        ((equal? letter 'v) 5)
        ((equal? letter 'x) 10)
        ((equal? letter 'l) 50)
        ((equal? letter 'c) 100)
        ((equal? letter 'd) 500)
        ((equal? letter 'm) 1000)
        (else 'huh?)))

(define (arabic s)
  (let ((fval (roman-value (first s))))
    (if (= (count s) 1)
        fval
        (let ((rst (bf s)))
          (+ ((if (> (roman-value (first rst)) fval) - +) fval)
             (arabic rst))))))

(and
  (equal? (arabic 'mcmlxxi) 1971)
  (equal? (arabic 'mlxvi) 1066))

