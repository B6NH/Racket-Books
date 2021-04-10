; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(amazify name)
  (word 'the-amazing- name))

(define(transform-beatles f)
  (every f '(john paul george ringo)))


(and
  (equal?(transform-beatles amazify)
         '(the-amazing-john the-amazing-paul
           the-amazing-george the-amazing-ringo))
  (equal?(transform-beatles butfirst)
         '(ohn aul eorge ingo)))
