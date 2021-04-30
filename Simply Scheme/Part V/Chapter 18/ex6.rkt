; Exercise 6

#lang racket

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (function-named-by oper)
  (cond ((equal? oper '+) +)
  ((equal? oper '-) -)
  ((equal? oper '*) *)
  ((equal? oper '/) /)
  (else (error "no such operator as" oper))))

(define (compute tree)
  (if (number? (datum tree))
      (datum tree)
      (let ((chldr (children tree)))
        ((function-named-by (datum tree))
         (compute (car chldr))
         (compute (cadr chldr))))))

(define (parse-scheme exp)
  (if (number? exp)
      (make-node exp '())
      (make-node (car exp)
                (list (parse-scheme (car (children exp)))
                   (parse-scheme (cadr (children exp)))))))

(and
  (equal? (parse-scheme '(* (+ 4 3) 2))
                        '(* (+ (4) (3)) (2)))
  (equal? (compute (parse-scheme '(* (+ 4 3) 2))) 14)
  (equal? (compute (parse-scheme '(* (- (* 22 (/ 16 8)) (+ 7 1)) 5 ))) 180)
  (equal? (compute (make-node 12 '())) 12)
  (equal? (compute (make-node '* (list (make-node 7 '())
                                      (make-node 8 '())))) 56)
  (equal? (compute (make-node '+
                    (list (make-node 7 '())
                          (make-node '* (list (make-node 3 '())
                                              (make-node 8 '()))))))
          31))

