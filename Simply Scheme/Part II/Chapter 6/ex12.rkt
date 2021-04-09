; Exercise 12

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define (vowel? letter)
  (member? letter '(a e i o u)))

(define(plural w)
  (let((lw(last w))(blw(bl w)))
    (if(and(equal? lw 'y)(not(vowel?(last blw))))
       (word blw 'ies)
       (word w(if(equal? lw 'x) 'e "")'s))))

(and(equal? (plural 'dragon) 'dragons)
    (equal? (plural 'butterfly) 'butterflies)
    (equal? (plural 'boy) 'boys)
    (equal? (plural 'box) 'boxes))
