; Exercise 12

#lang racket

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (leaf? node)
  (null? (children node)))

(define (forest-reduce forest fn)
  (if (empty? forest)
      (fn)
      (fn (tree-reduce fn (car forest))
          (forest-reduce (cdr forest) fn))))

(define (tree-reduce fn tree)
  (if (leaf? tree)
      (datum tree)
      (fn (datum tree)
          (forest-reduce (children tree) fn))))


(and
  (equal?
    (tree-reduce
      +
      (make-node 3 (list (make-node 4 '())
                         (make-node 7 '())
                         (make-node 2 (list (make-node 3 '())
                                            (make-node 8 '()))))))
    27)
  (equal?
    (tree-reduce
      *
      (make-node 3 (list (make-node 4 '())
                         (make-node 7 '())
                         (make-node 2 (list (make-node 3 '())
                                            (make-node 8 '()))))))
    4032))
