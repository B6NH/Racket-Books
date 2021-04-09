; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (vowel? letter) (member? letter '(a e i o u)))

(define (gertrude wd)
  (let ((s(if (vowel? (first wd)) 'an 'a)))
    (se s wd 'is s wd 'is s wd)))


(gertrude 'rose)
(gertrude 'iguana)
