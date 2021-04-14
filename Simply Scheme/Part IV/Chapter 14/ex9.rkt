; Exercise 9

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (helper w s index)
  (cond
    ((empty? s) #f)
    ((equal? (first s) w) index)
    (else (helper w (bf s) (+ index 1)))))

(define (location w s)
  (helper w s 1))



(and
  (equal? (location 'me '(you never give me your money))
          4)
  (equal? (location 'stone '(write the procedure))
          #f))



