; Exercise 12

#lang racket
(require (planet dyoo/simply-scheme:2:2))

; Empty sentence
(butfirst '(emerald))
(first '(() a))
(last '(b ()))

; Empty word
(butfirst 'a)
(butlast 'a)
