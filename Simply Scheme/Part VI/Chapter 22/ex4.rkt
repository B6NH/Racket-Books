; Exercise 4

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (char-helper port n)
  (let ((str (read-string port)))
    (if (eof-object? str)
        n
        (char-helper port (+ (count str) n)))))

(define (count-characters file)
  (let ((port (open-input-file file)))
    (let ((result (char-helper port 0)))
      (begin
        (close-input-port port)
        result))))


(define alen 37)
(define blen 33)
(define clen 55)

(and
  (equal? (count-characters "file_a") 37)
  (equal? (count-characters "file_b") 33)
  (equal? (count-characters "file_c") 55)
  (equal? (count-characters "clines") 54)
  (equal? (count-characters "out_file") (+ alen blen clen)))
