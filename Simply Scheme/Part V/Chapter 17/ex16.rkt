; Exercise 16

#lang racket

(define (member? n lst)
  (if (member n lst) #t #f))

(define (valid-value? v)
  (or (number? v)
      (and (list? v)
           (valid-infix? v))))

(define (helper lst)
  (and (not (or (empty? lst)
                (empty? (cdr lst))))
       (member? (car lst) '(+ - * /))
       (valid-value? (cadr lst))
       (or (empty? (cddr lst))
           (helper (cddr lst)))))

(define (valid-infix? lst)
  (and (not (empty? lst))
       (valid-value? (car lst))
       (helper (cdr lst))))

(and
  (equal? (valid-infix? '()) #f)
  (equal? (valid-infix? '(5)) #f)
  (equal? (valid-infix? '(1 2)) #f)
  (equal? (valid-infix? '(6 7 8)) #f)
  (equal? (valid-infix? '(2 +)) #f)
  (equal? (valid-infix? '(3 * *)) #f)
  (equal? (valid-infix? '(4 * * 9)) #f)
  (equal? (valid-infix? '(+ 7)) #f)
  (equal? (valid-infix? '(5 + 7 *)) #f)
  (equal? (valid-infix? '(4 + 3 * (5 2))) #f)
  (equal? (valid-infix? '(4 + 3 * (5 2) +)) #f)
  (equal? (valid-infix? '(6 + 7)) #t)
  (equal? (valid-infix? '(5 * 2 / 4 + 3)) #t)
  (equal? (valid-infix? '(4 + 3 * (5 - 2))) #t)
  (equal? (valid-infix? '((9 * 8) / 4 + 3)) #t)
  (equal? (valid-infix? '((2 * (2 * 4 + 5)) / 4 + 3)) #t))

