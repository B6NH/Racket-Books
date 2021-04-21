; Exercise 6

#lang racket

(define (myappend a b)
  (if (empty? a)
      b
      (cons (car a) (myappend (cdr a) b))))


(define (myappend2 a . restlist)
  (if (empty? restlist)
      a
      (apply myappend2
        (cons (myappend a (car restlist))
              (cdr restlist)))))


(and
  (equal? (append '(1 2 3) '(4 5 6))
          (myappend '(1 2 3) '(4 5 6)))
  (equal? (append '(1 2 3) '(4 5 6) '(7 8 9))
          (myappend2 '(1 2 3) '(4 5 6) '(7 8 9))))

