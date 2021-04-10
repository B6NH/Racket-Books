; Exercise 11

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(base-grade g)
  (cond
    ((equal? g 'A)4.)
    ((equal? g 'B)3.)
    ((equal? g 'C)2.)
    ((equal? g 'D)1.)
    ((equal? g 'E)0.)))
    
(define(grade-modifier x)
  (if(=(count x)2)
     (let((ls(last x)))
       (cond
        ((equal? ls '-) -.33)
        ((equal? ls '+) +.33)))
     0))

(define(gpa lst)
  (/ (accumulate +
       (every (lambda(x)
         (+ (base-grade(first x))
            (grade-modifier x))) lst))
     (count lst)))


(and
  (equal? (gpa '(A A+ B+ B)) (/(+ 4 4 0.33 3 0.33 3)4))
  (equal? (gpa '(A A- C C+)) (/(+ 4 4 -.33 2 2 .33)4)))

