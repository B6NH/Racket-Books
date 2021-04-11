; Exercise 15

#lang racket

(define(type-check fn pred)
  (lambda(x)
    (if(pred x)
       (fn x)
       #f)))

(define safe-sqrt (type-check sqrt number?))


(and
  (equal? (safe-sqrt 16) 4)
  (equal? (safe-sqrt 'sarsaparilla) #f))
