; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(symbol x)
  (word
    x
    (cond
      ((equal? x 'a) 'lpha)
      ((equal? x 'b) 'ravo)
      ((equal? x 'c) 'harlie)
      ((equal? x 'd) 'elta)
      ((equal? x 'e) 'cho)
      ((equal? x 'f) 'oxtrot)
      ((equal? x 'g) 'olf)
      ((equal? x 'h) 'otel)
      ((equal? x 'i) 'ndia)
      ((equal? x 'j) 'uliett)
      ((equal? x 'k) 'ilo)
      ((equal? x 'l) 'ima)
      ((equal? x 'm) 'ike)
      ((equal? x 'n) 'ovember)
      ((equal? x 'o) 'scar)
      ((equal? x 'p) 'apa)
      ((equal? x 'q) 'uebec)
      ((equal? x 'r) 'omeo)
      ((equal? x 's) 'ierra)
      ((equal? x 't) 'ango)
      ((equal? x 'u) 'niform)
      ((equal? x 'v) 'ictor)
      ((equal? x 'w) 'hiskey)
      ((equal? x 'x) '-ray)
      ((equal? x 'y) 'ankee)
      ((equal? x 'z) 'ulu))))

(define(words w)
  (every symbol w))
  
(and
  (equal? (words 'cab) '(charlie alpha bravo))
  (equal?(words 'game) '(golf alpha mike echo)))
