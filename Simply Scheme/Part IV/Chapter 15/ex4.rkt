; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define (helper sub wd)
  (or (empty? sub)
      (and (not (empty? wd))
           (equal? (first sub) (first wd))
           (helper (bf sub) (bf wd)))))


(define (substring? sub wd)
  (and (not (empty? wd))
       (or (helper sub wd)
           (substring? sub (bf wd)))))

(and
  (equal? (substring? 'ssip 'mississippi) #t)
  (equal? (substring? 'aps 'rhapsody) #t)
  (equal? (substring? 'e 'red) #t)
  (equal? (substring? 'agon 'dragon) #t)
  (equal? (substring? 'symphony 'symphony) #t)
  (equal? (substring? 'proc 'procedure) #t)
  (equal? (substring? 'misip 'mississippi) #f)
  (equal? (substring? 'mountain 'mount) #f)
  (equal? (substring? 'lc 'black) #f))

