; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(european-time t)
  (let((h(first t)))
    (let((te(equal? h 12)))
      (if(equal? (last t) 'am)
        (if te 24 h)
        (+ 12 (if te 0 h))))))

(define(american-time t)
  (if(> t 12)
     (se (- t 12) (if(equal? t 24) 'am 'pm))
     (se t (if(equal? t 12) 'pm 'am))))
  
(equal? (european-time '(1 am)) 1)
(equal? (european-time '(8 am)) 8)
(equal? (european-time '(12 pm)) 12)
(equal? (european-time '(1 pm)) 13)
(equal? (european-time '(4 pm)) 16)
(equal? (european-time '(12 am)) 24)

(displayln "---------")

(equal? (american-time 1) '(1 am))
(equal? (american-time 8) '(8 am))
(equal? (american-time 12) '(12 pm))
(equal? (american-time 13) '(1 pm))
(equal? (american-time 16) '(4 pm))
(equal? (american-time 24) '(12 am))
