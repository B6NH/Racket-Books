; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (words-helper port n)
  (let ((line (read-line port)))
    (if (eof-object? line)
        n
        (words-helper port (+ n (length line))))))

(define (count-words file)
  (let ((port (open-input-file file)))
    (let ((result (words-helper port 0)))
      (begin
        (close-input-port port)
        result))))


(and
  (equal? (count-words "file_a") 6)
  (equal? (count-words "file_b") 5)
  (equal? (count-words "file_c") 9)
  (equal? (count-words "clines") 14))
