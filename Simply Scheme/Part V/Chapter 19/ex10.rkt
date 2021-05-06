; Exercise 10

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (double x)
  (* x 2))

(define (addx wd)
  (word wd 'xxx))

(define (deep-map f structure)
  (cond
    ((word? structure) (f structure))
    ((null? structure) '())
    (else (cons (deep-map f (car structure))
                (deep-map f (cdr structure))))))

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (map-forest fn forest)
  (if (empty? forest)
      '()
      (cons (tree-map fn (car forest))
            (map-forest fn (cdr forest)))))

(define (tree-map fn tree)
  (make-node (fn (datum tree))
             (map-forest fn (children tree))))


(and
  (equal? (deep-map double '(5 6 2)) '(10 12 4))
  (equal? (deep-map double '(3 (6 9) (5 (22 2))))
          '(6 (12 18) (10 (44 4))))
  (equal? (tree-map addx (make-node 'root '()))
          '(rootxxx))
  (equal? (tree-map addx
            (make-node 'root
              (list (make-node 'a '())
                    (make-node 'b '()))))
          '(rootxxx (axxx) (bxxx)))
  (equal? (tree-map addx
            (make-node 'root
              (list (make-node 'a (list (make-node 'c '())
                                        (make-node 'd '())))
                    (make-node 'b '()))))
          '(rootxxx (axxx (cxxx) (dxxx)) (bxxx))))
