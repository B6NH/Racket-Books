; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (helper wd)
  (or (<= (count wd) 1)
      (and (equal? (first wd) (last wd))
           (helper (bl (bf wd))))))

(define (palindrome? s)
  (helper (accumulate word s)))

(and
  (equal? (palindrome? '()) #t)
  (equal? (palindrome? '(v)) #t)
  (equal? (palindrome? '(flee to me remote elf)) #t)
  (equal? (palindrome? '(flee to me remote control)) #f)
  (equal? (palindrome? '(no lemon no melon)) #t)
  (equal? (palindrome? '(eva can i see bees in a cave)) #t)
  (equal? (palindrome? '(raz czart raz czar)) #t)
  (equal? (palindrome? '(do not reverse any words)) #f))
