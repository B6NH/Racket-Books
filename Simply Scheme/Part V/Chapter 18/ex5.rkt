; Exercise 5

#lang racket

(define (leaf? node)
  (null? (children node)))

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (prune-forest forest)
  (if (empty? forest)
      '()
      (let ((fst (car forest))
            (pruned (prune-forest (cdr forest))))
        (if (leaf? fst)
          pruned
          (cons (make-node (datum fst)
                           (prune-forest (children fst)))
                 pruned)))))

(define (prune tree)
  (let ((chldr (children tree)))
    (if (empty? chldr)
        #f
        (make-node (datum tree)
                   (prune-forest chldr)))))

(and
  (equal? (prune (make-node 'root '())) #f)
  (equal? (prune (make-node 'root (list (make-node 'a '())
                                        (make-node 'b '())))) '(root))
  (equal? (prune (make-node 'root
                            (list (make-node 'a '())
                                  (make-node 'b
                                    (list (make-node 'c '()))))))
          '(root (b)))


  (equal?
    (prune
      (make-node 'root
        (list (make-node 'a (list (make-node 'e (list (make-node 'f '())))
                                  (make-node 'g '())))
              (make-node 'b '())
              (make-node 'c '())
              (make-node 'd
                (list (make-node 'h (list (make-node 'j '())))
                      (make-node 'k
                        (list (make-node 'l '())
                              (make-node 'm
                                (list (make-node 'n '()))))))))))
    '(root (a (e)) (d (h) (k (m))))))
