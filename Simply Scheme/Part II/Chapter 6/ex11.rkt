; Exercise 11

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(div-by x y)
  (zero?(remainder x y)))

(define(f29 d m y)
  (or(not(= m 2))
     (and(div-by y 4)
         (or(not(div-by y 100))
            (div-by y 400)))))

(define(valid-month m)
  (and(> m 0)(< m 13)))

(define(valid-day d m y)
  (cond
    ((= d 31)(if(odd? m)(< m 8)(>= m 8)))
    ((= d 30)(not(= m 2)))
    ((= d 29)(f29 d m y))
    (else(and(> d 0)(< d 29)))))
    

(define(valid-date? month day year)
  (and(valid-month month)
      (valid-day day month year)))
  

(and
  (equal? (valid-date? 10 4 1949) #t)
  (equal? (valid-date? 20 4 1776) #f)
  (equal? (valid-date? 5 0 1992) #f)
  (equal? (valid-date? 5 -2 1992) #f)
  (equal? (valid-date? 2 29 1900) #f)
  (equal? (valid-date? 2 29 2000) #t)
  (equal? (valid-date? 6 36 2000) #f)
  (equal? (valid-date? 6 31 2000) #f)
  (equal? (valid-date? 7 31 2000) #t)
  (equal? (valid-date? 8 31 2000) #t)
  (equal? (valid-date? 9 31 2000) #f))
