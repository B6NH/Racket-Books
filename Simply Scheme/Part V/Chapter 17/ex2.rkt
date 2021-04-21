; Exercise 2

#lang racket

(define (f1 a b)
  (list(list (cadr a)
             (caddr a)
             (car b))))

(define (f2 a b)
  (list (cdr a) (cadr b)))

(define (f3 a b)
  (append a a))

(define (f4 a b)
  (list (list (car a) (car b))
        (append (cdr a) (cdr b))))

(and
  (equal? (f1 '(a b c) '(d e f)) '((b c d)))
  (equal? (f2 '(a b c) '(d e f)) '((b c) e))
  (equal? (f3 '(a b c) '(d e f)) '(a b c a b c))
  (equal? (f4 '(a b c) '(d e f)) '((a d) (b c e f))))
