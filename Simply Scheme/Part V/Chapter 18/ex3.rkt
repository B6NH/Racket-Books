; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (leaf datum)
  (make-node datum '()))

(define (leaf? node)
  (null? (children node)))

(define (cities name-list)
  (map leaf name-list))

(define (helper lst mx)
  (cond
    ((empty? lst) mx)
    ((> (car lst) mx)
     (helper (cdr lst) (car lst)))
    (else (helper (cdr lst) mx))))

(define (max-value lst)
  (helper (cdr lst) (car lst)))

(define (depth tree)
  (if (leaf? tree)
      1
      (+ 1 (max-value (map depth (children tree))))))

(define (count-nodes tree)
  (if (empty? (children tree))
      1
      (+ 1 (accumulate + (map count-nodes (children tree))))))

(define world-tree
  (make-node
    'world
    (list (make-node
            'italy
            (cities '(venezia riomaggiore firenze roma)))
          (make-node
            '(united states)
              (list (make-node
                      'california
                      (cities '(berkeley (san francisco) gilroy)))
                    (make-node
                      'massachusetts
                      (cities '(cambridge amherst sudbury)))
                    (make-node
                      'ohio
                      (cities '(kent)))))
          (make-node 'zimbabwe (cities '(harare hwange)))
          (make-node 'china
            (cities '(beijing shanghai guangzhou suzhou)))
          (make-node
           '(great britain)
             (list
               (make-node 'england (cities '(liverpool)))
               (make-node 'scotland
                 (cities '(edinburgh glasgow (gretna green))))
               (make-node 'wales (cities '(abergavenny)))))
          (make-node
            'australia
              (list
                (make-node 'victoria (cities '(melbourne)))
                (make-node '(new south wales) (cities '(sydney)))
                (make-node 'queensland
            (cities '(cairns (port douglas))))))
          (make-node 'honduras (cities '(tegucigalpa))))))


(and
  (equal? (count-nodes (make-node 'root '())) 1)
  (equal? (count-nodes (make-node 'root (list (make-node 'a '())))) 2)
  (equal? (count-nodes (make-node 'root (list (make-node 'a '())
                                              (make-node 'b '())))) 3)
  (equal? (count-nodes (make-node
                         'root
                         (list (make-node 'a (list (make-node 'c '())))
                               (make-node 'b '())))) 4)
  (equal? (count-nodes world-tree) 44)
  (equal? (depth (make-node 'root '()))
          1)
  (equal? (depth (make-node 'root (list (make-node 'a '())
                                        (make-node 'b '()))))
          2)
  (equal? (depth (make-node 'root (list (make-node 'a (cities '(b b b)))
                                        (make-node 'b (cities '(c c c))))))
          3)
  (equal? (depth world-tree) 4)
  (equal? (depth
    (make-node
      'root
      (list (make-node
              'x
              (list (make-node
                      'y
                      (list (make-node
                              'z
                              (list (make-node 'm
                                                '())))))))))) 5))
