; Exercise 13

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (deep-reduce fn lst)
  (if (empty? lst)
      (fn)
      (let ((f (car lst)))
        (fn (if (list? f)
                (deep-reduce fn f)
                f)
            (deep-reduce fn (cdr lst))))))


(and
  (equal? (deep-reduce word '(r ((a (m b) (l)) (e (r))))) 'rambler)
  (equal? (deep-reduce + '(1 2 3 4 5)) 15)
  (equal? (deep-reduce * '((1 (2 3)) (4 5))) 120))
