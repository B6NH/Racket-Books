; Exercise 4

#lang racket

;; The existence of a command is checked before it is executed
;; and 'get-command' function does not have to deal with error handling.

;; The 'get-function' function is called if the expression is a list
;; (predicate invocation?) but there is no guarantee that the needed procedure exists.
;; For this reason, there is error handling code inside it.

#t
