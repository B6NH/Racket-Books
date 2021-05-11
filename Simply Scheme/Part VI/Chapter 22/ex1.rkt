; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (helper file in out)
  (let ((line (read-line in)))
    (if (eof-object? line)
        'done
        (begin
          (show-line line out)
          (helper file in out)))))

(define (copy-file file port)
  (let ((in-port (open-input-file file)))
    (begin
      (helper file in-port port)
      (close-input-port in-port)
      'done)))

(define (copy-files files port)
  (if (empty? files)
      'done
      (begin
        (copy-file (car files) port)
        (copy-files (cdr files) port))))

(define (concatenate files output)
  (let ((port (open-output-file output)))
    (begin
      (copy-files files port)
      (close-output-port port))))


(concatenate '("file_a" "file_b" "file_c") "out_file")
