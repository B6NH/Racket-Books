; Exercise 13

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(ltd l)
  (cond
    ((member? l 'abc)2)
    ((member? l 'def)3)
    ((member? l 'ghi)4)
    ((member? l 'jkl)5)
    ((member? l 'mno)6)
    ((member? l 'pqrs)7)
    ((member? l 'tuv)8)
    ((member? l 'wxyz)9)))

(define(phone-unspell lst)
  (accumulate word (every ltd lst)))


(and
  (equal? (phone-unspell 'popcorn) 7672676)
  (equal? (phone-unspell 'coffee) 263333))
