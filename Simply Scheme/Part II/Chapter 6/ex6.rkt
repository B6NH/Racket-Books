; Exercise 6

#lang racket

(define(teen? age)
  (and(>= age 13)(<= age 19)))
  
  
(teen? 6)
(teen? 12)
(displayln "---")
(teen? 13)
(teen? 15)
(teen? 18)
(teen? 19)
(displayln "---")
(teen? 20)
(teen? 21)
