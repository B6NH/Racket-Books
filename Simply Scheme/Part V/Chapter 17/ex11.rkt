; Exercise 11

#lang racket

(define (helper elm lst)
  (and (not (empty? lst))
       (or (equal? (car lst) elm)
           (helper elm (cdr lst)))))

(define (before-in-list? lst a b)
  (and (not (empty? lst))
       (let ((rst (cdr lst)))
         (if (equal? (car lst) a)
             (helper b rst)
             (before-in-list? rst a b)))))


(and
  (equal? (before-in-list? '(back in the ussr) 'in 'ussr) #t)
  (equal? (before-in-list? '(back in the ussr) 'the 'back) #f)
  (equal? (before-in-list? '(czy pamietasz) 'czy 'to) #f)
  (equal? (before-in-list? '(jak sie masz) 'co 'masz) #f)
  (equal? (before-in-list? '(album completo) 'completo 'album) #f)
  (equal? (before-in-list? '(nocny dyzur) 'locked 'within) #f))
