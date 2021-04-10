; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(vowel? letter)
  (member? letter '(a e i o u)))


(keep vowel? 'birthday)
(every first '(golden slumbers))
(first '(golden slumbers))
(every last '(little child))
(accumulate word(every last '(little child)))
(every + '(2 3 4 5))
(accumulate + '(2 3 4 5))
