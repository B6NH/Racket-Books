; Exercise 12

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (flatten lst)
  (cond
    ((empty? lst) '())
    ((word? (car lst))
     (cons (car  lst)
           (flatten (cdr lst))))
    (else
      (append (flatten (car lst))
              (flatten (cdr lst))))))



(equal? (flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))
        '(a b c d e f g h i j k))
