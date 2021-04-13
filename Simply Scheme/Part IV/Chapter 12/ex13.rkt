; Exercise 13

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define sec-in-min 60.)
(define sec-in-hour (* sec-in-min 60))
(define sec-in-day (* sec-in-hour 24))
(define sec-in-week (* sec-in-day 7))
(define sec-in-month (* sec-in-week 4))
(define sec-in-year (* sec-in-month 12))
(define sec-in-century (* sec-in-year 100))

(define(conv t)
  (cond
    ((< t sec-in-hour)(se (floor (/ t sec-in-min)) 'minutes))
    ((< t sec-in-day)(se (floor (/ t sec-in-hour)) 'hours))
    ((< t sec-in-week)(se (floor (/ t sec-in-day)) 'days))
    ((< t sec-in-month)(se (floor (/ t sec-in-week)) 'weeks))
    ((< t sec-in-year)(se (floor (/ t sec-in-month)) 'months))
    ((< t sec-in-century)(se (floor (/ t sec-in-year)) 'years))
    (else (se (floor (/ t sec-in-century)) 'centuries))))

(define(rem t converted)
  (let ((lc(last converted)))
    (- t
      (* (first converted)
         (cond
           ((equal? lc 'minutes) sec-in-min)
           ((equal? lc 'hours) sec-in-hour)
           ((equal? lc 'days) sec-in-day)
           ((equal? lc 'weeks) sec-in-week)
           ((equal? lc 'months) sec-in-month)
           ((equal? lc 'years) sec-in-year)
           ((equal? lc 'centuries) sec-in-century))))))

(define(describe-time t)
  (if (< t sec-in-min)
      (se t 'seconds)
      (let ((ctime (conv t)))
        (let ((rtime (rem t ctime)))
          (se ctime (describe-time rtime))))))

(and
  (equal? (describe-time 100)
          '(1.0 minutes 40.0 seconds))
  (equal? (describe-time 22222)
          '(6.0 hours 10.0 minutes 22.0 seconds))
  (equal? (describe-time 4967189641)
          '(1.0 centuries 71.0 years 1.0 months 6.0 days
            14.0 hours 54.0 minutes 1.0 seconds)))

