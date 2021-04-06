; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define(acronym phrase)
  (accumulate word (every first(keep real-word? phrase))))
  
(define(real-word? wd)
  (not(member? wd '(a the an in of and for to with))))


(displayln(acronym '(american civil liberties union)))
(displayln(acronym '(reduced instruction set computer)))
(displayln(acronym '(quod erat demonstrandum)))

(displayln(acronym '(united states of america)))
(displayln(acronym '(structure and interpretation of computer programs)))
(displayln(acronym '(association for computing machinery)))
