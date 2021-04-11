; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(lambda (x) (+ (* x 3) 4))
((lambda (x) (+ (* x 3) 4)) 10)
(every (lambda (wd) (word (last wd) (bl wd)))
         '(any time at all))

; error
;((lambda (x) (+ x 3)) 10 15)
