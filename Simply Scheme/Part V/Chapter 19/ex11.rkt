; Exercise 11

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (add2 x)
  (+ x 2))

(define (helper fn times arg)
  (if (zero? times)
      arg
      (helper fn (- times 1) (fn arg))))

(define (myrepeated fn times)
  (lambda (x)
    (helper fn times x)))


(and
  (equal? ((repeated add2 3) 10) 16)
  (equal? ((myrepeated add2 3) 10) 16)
  (equal? ((myrepeated (lambda (x) (* x 2)) 5) 2) 64))
