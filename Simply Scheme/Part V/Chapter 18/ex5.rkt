; Exercise 5

#lang racket

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (prune tree)
  #t)


(prune (make-node 'root '()))


