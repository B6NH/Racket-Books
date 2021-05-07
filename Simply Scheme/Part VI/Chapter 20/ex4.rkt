; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (converse)
  (begin
    (show "Hello. I'm the computer. What's your name? ")
    (let ((name (car (read-line))))
      (begin
        (display "Hi ")
        (display name)
        (show ". How are you?")
        (read-line)
        (show "Glad to hear that.")))))

(converse)

