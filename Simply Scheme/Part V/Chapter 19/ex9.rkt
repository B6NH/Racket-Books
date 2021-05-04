; Exercise 9

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (earliest-word sent fn)
  (earliest-helper (first sent) (bf sent) fn))

(define (earliest-helper so-far rest fn)
  (if (empty? rest)
      so-far
      (let ((fst (first rest)))
        (earliest-helper (if (fn so-far fst) so-far fst) (bf rest) fn))))

(define (remove-once wd sent)
  (if (empty? sent)
      '()
      (let ((fst (first sent)) (rst (bf sent)))
        (if (equal? wd fst)
            (bf sent)
            (se fst (remove-once wd rst))))))


(define (mysort sent fn)
  (if (empty? sent)
      '()
      (let ((wd (earliest-word sent fn)))
        (se wd (mysort (remove-once wd sent) fn)))))


(and
  (equal? (mysort '(4 23 7 5 16 3) <) '(3 4 5 7 16 23))
  (equal? (mysort '(4 23 7 5 16 3) >) '(23 16 7 5 4 3))
  (equal? (mysort '(john paul george ringo) before?)
          '(george john paul ringo)))
