; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(vowel? letter)
  (member? letter '(a e i o u)))
  
(define(square x)
  (* x x))


(every last '(algebra purple spaghetti tomato gnu))
(keep number? '(one two three four))
(accumulate * '(6 7 13 0 9 42 17))
(member? 'h (keep vowel? '(t h r o a t)))
(every square (keep even? '(87 4 7 12 0 5)))
(accumulate word (keep vowel? (every first '(and i love her))))
((repeated square 0) 25)
(every (repeated bl 2) '(good day sunshine))
