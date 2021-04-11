; Exercise 11

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(unabbrev a b)
  (every
    (lambda(x)
      (if(number? x)
         (item x b)
         x))
    a))


(and
  (equal? (unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey))
          '(john bill wayne fred joey))
  (equal? (unabbrev '(i 3 4 tell 2) '(do you want to know a secret?))
          '(i want to tell you)))
