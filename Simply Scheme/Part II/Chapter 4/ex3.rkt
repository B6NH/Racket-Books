; Exercise 3

#lang racket

(define (f x y) (- y x))

(define (identity x) x)

(define (three x) 3)

(define (seven) 7)

(define (magic n)
  (- (/ (+ (+ (* 3 n)
              13)
           (- n 1))
        4)
     3))
     


; (- 8 5)
(f 5 8)

; 7
(identity 7)

; 3
(three 9)

; 7
(seven)

; (- (/ (+ (+ (* 3 12) 13) (- 12 1)) 4) 3)
; (- (/ (+ 49 11) 4) 3)
; (- 15 3)
(magic 12)
