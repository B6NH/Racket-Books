; Exercise 8

#lang racket

(define (mymember a lst)
  (if (empty? lst)
      #f
      (let ((fst (car lst)) (rst (cdr lst)))
        (if (equal? fst a)
            (cons fst rst)
            (mymember a rst)))))

(define lsta '(5 33 4 7))
(define lstb '(6 44 12 17 82))
(define lstc '())
(define n 12)

(and
  (equal? (member n lsta)
          (mymember n lsta))
  (equal? (member n lstb)
          (mymember n lstb))
  (equal? (member n lstc)
          (mymember n lstc)))


