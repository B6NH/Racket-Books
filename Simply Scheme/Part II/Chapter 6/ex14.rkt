; Exercise 14

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define sec-in-min 60.)
(define sec-in-hour (* sec-in-min 60))
(define sec-in-day (* sec-in-hour 24))
(define sec-in-week (* sec-in-day 7))
(define sec-in-month (* sec-in-week 4))
(define sec-in-year (* sec-in-month 12))
(define sec-in-century (* sec-in-year 100))

(define(describe-time t)
  (cond
    ((< t sec-in-min)(se t 'seconds))
    ((< t sec-in-hour)(se (/ t sec-in-min)'minutes))
    ((< t sec-in-day)(se (/ t sec-in-hour)'hours))
    ((< t sec-in-week)(se (/ t sec-in-day)'days))
    ((< t sec-in-month)(se (/ t sec-in-week)'weeks))
    ((< t sec-in-year)(se (/ t sec-in-month)'months))
    ((< t sec-in-century)(se (/ t sec-in-year)'years))
    (else(se (/ t sec-in-century)'centuries))))


(describe-time 45)
(describe-time 930)
(describe-time 8640)
(describe-time 280800)
(describe-time 18144000)
(describe-time 130636800)
(describe-time 7257600000)
(describe-time 30000000000)
