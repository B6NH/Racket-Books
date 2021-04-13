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

(define(helper a b)
  (let ((av (roman-value a))
        (bv (roman-value b)))
    ((if (> bv av) - +) av)))

(define(arabic rmn)
  (if (= (count rmn) 1)
      (roman-value rmn)
      (+ (helper (first rmn) (second rmn))
         (arabic (bf rmn)))))

(and
  (equal? (arabic 'mcmlxxi) 1971)
  (equal? (arabic 'mlxvi) 1066))
