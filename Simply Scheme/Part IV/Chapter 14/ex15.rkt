; Exercise 15

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (merge a b)
  (cond
    ((empty? a) b)
    ((empty? b) a)
    (else
      (let ((fa (first a))
            (fb (first b)))
        (if (> fa fb)
            (se fb (merge a (bf b)))
            (se fa (merge (bf a) b)))))))



(equal? (merge '(4 7 18 40 99) '(3 6 9 12 24 36 50))
        '(3 4 6 7 9 12 18 24 36 40 50 99))
