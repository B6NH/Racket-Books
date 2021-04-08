; Exercise 18

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (ends word)
  (word (first word) (last word)))
 

; (ends 'john)
; application: not a procedure;
;  expected a procedure that can be applied to arguments
