; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (is-free n position)
  (equal? (item n position) '_))

(define (message-ask message position)
  (begin (display message)
         (let ((input (read)))
         (test-input input position))))

(define (test-input input position)
  (cond
    ((not (number? input))
     (message-ask "Not a number: " position))
    ((or (< input 1) (> input 9))
     (message-ask "Enter number between 1 and 9: " position))
    ((not (is-free input position))
     (message-ask "Position is not free: " position))
    (else input)))

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (let ((input (read)))
    (test-input input position)))

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

