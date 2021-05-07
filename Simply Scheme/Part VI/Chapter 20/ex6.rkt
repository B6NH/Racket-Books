; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (read))

(define (print-position position)
  (print-row (subword position 1 3))
  (show "-+-+-")
  (print-row (subword position 4 6))
  (show "-+-+-")
  (print-row (subword position 7 9))
  (newline))

(define (print-row row)
  (maybe-display (first row))
  (display "|")
  (maybe-display (first (bf row)))
  (display "|")
  (maybe-display (last row))
  (newline))

(define (maybe-display letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (subword wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
    wd)))


(print-position '_x_oo__xx)

(ask-user '____X____ 'O)



