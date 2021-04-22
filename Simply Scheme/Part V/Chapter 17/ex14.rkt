; Exercise 14

#lang racket

(define (myitem n lst)
  (if (= n 1)
      (car lst)
      (myitem (- n 1)
              (cdr lst))))

(define (branch a b)
  (let ((next (myitem (car a) b)) (rst (cdr a)))
    (if (empty? rst)
        next
        (branch rst
                next))))

(and
  (equal? (branch '(3) '((a b) (c d) (e f) (g h)))
          '(e f))
  (equal? (branch '(3 2) '((a b) (c d) (e f) (g h)))
          'f)
  (equal? (branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m)))
          'h))
