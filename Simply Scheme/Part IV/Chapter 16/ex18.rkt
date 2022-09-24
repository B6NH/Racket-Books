; Exercise 18

#lang racket

; (1) With function that doesn't compare words ('chop-leading-substring2'):

; Word doesn't match
; (match '(!abc or !abc) '(tea or coffee)) -> '(abc tea !)

; Three words don't match
; (match '(*abc or &abc) '(earth venus mars or night)) -> '(abc earth venus mars !)

; (2) Replace 'match-using-known-values' call with 'known-values' in 'already-known-match' function:

; Pattern is too short
; (match '(!abc or !abc) '(purple or purple star)) -> '(abc purple !)

; Placeholder '!end' doesn't match
; (match '(!abc and !abc !end) '(green and green)) -> '(abc green !)

; (3) Without 'length-ok?' (comment out in 'match-special' function):

; Length of 'abc' cannot be greater than 1
; (match '(*abc or !abc) '(tea tea tea or tea tea tea)) -> '(abc tea tea tea !)

; Placeholder 'abc' must not be empty
; (match '(*abc or !abc) '(or)) -> '(abc !)

#t
