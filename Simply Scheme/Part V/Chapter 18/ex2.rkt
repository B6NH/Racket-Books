; Exercise 2

#lang racket

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (children2 node)
  (cadr node))

(define (make-node2 datum children)
  (list datum children))


(define nodeA (make-node 'root '(a b c d)))
(define nodeB (make-node2 'root '(a b c d)))

(and
  (equal? nodeA '(root a b c d))
  (equal? nodeB '(root (a b c d)))
  (equal? (datum nodeA) 'root)
  (equal? (datum nodeB) 'root)
  (equal? (children nodeA) '(a b c d))
  (not (equal? (children nodeB) '(a b c d)))
  (equal? (children2 nodeB) '(a b c d))
  (not (equal? (children2 nodeA) '(a b c d))))


