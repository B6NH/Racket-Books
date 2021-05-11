; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (count-helper port n)
  (if (eof-object? (read-line port))
      n
      (count-helper port (+ n 1))))

(define (count-lines file)
  (let ((port (open-input-file file)))
    (let ((result (count-helper port 0)))
      (begin
        (close-input-port port)
        result))))


(and
  (equal? (count-lines "file_a") 2)
  (equal? (count-lines "file_c") 3)
  (equal? (count-lines "clines") 6))

