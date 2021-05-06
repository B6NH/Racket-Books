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


(define (mymergesort lst fn)
  (if (<= (length lst) 1)
      lst
      (mymerge (mymergesort (one-half lst) fn)
               (mymergesort (other-half lst) fn)
               fn)))

(define (mymerge left right fn)
  (cond
    ((empty? left) right)
    ((empty? right) left)
    ((fn (car left) (car right))
     (cons (car left)
           (mymerge (cdr left) right fn)))
    (else
      (cons (car right)
            (mymerge left (cdr right) fn)))))

(define (one-half lst)
  (if (<= (length lst) 1)
      lst
      (cons (first lst)
            (one-half (cddr lst)))))

(define (other-half lst)
  (if (<= (length lst) 1)
      '()
      (cons (cadr lst)
          (other-half (cddr lst)))))


(define numbers '(4 23 7 5 16 3))
(define nasc '(3 4 5 7 16 23))
(define ndsc '(23 16 7 5 4 3))
(define names '(john paul george ringo))
(define nsorted '(george john paul ringo))

(and
  (equal? (mysort numbers <) nasc)
  (equal? (mysort numbers >) ndsc)
  (equal? (mysort names before?) nsorted)
  (equal? (mymergesort numbers >) ndsc)
  (equal? (mymergesort numbers <) nasc)
  (equal? (mymergesort names before?) nsorted))
