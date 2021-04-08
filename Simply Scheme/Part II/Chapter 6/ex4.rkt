; Exercise 4

#lang racket

(define (utensil meal)
  (cond ((equal? meal 'chinese) 'chopsticks)
  (else 'fork)))
  
(define (utensil2 meal)
  (if (equal? meal 'chinese) 'chopsticks 'fork))
  

(utensil 'chinese)
(utensil 'polish)

(utensil2 'chinese)
(utensil2 'polish)
