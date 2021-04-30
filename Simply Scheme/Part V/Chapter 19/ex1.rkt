; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define names '((john lennon) (paul mccartney)
                (george harrison) (ringo starr)))

(define lnames '(lennon mccartney harrison starr))

(and
  (equal? (every cdr names) lnames)
  (equal? (map cdr names) '((lennon) (mccartney) (harrison) (starr)))
  (equal? (every cadr names) lnames)
  (equal? (map cadr names) lnames))

