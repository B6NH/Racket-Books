; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (process in-port wd)
  (let ((line (read-line in-port)))
    (if (eof-object? line)
        'done
        (begin
          (if (member wd line)
              (show-line line)
              'done)
          (process in-port wd)))))

(define (lookup file wd)
  (let ((in-port (open-input-file file)))
    (begin
      (process in-port wd)
      (close-input-port in-port))))



(lookup "look" 'heat)
