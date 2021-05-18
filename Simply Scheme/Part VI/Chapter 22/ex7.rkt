; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (helper in-port count)
  (let ((line (read-line in-port)))
    (if (eof-object? line)
        'done
        (begin
          (show-line line)
          (if (= 1 count)
              line
              (helper in-port (- count 1)))))))

(define (show-all-lines in-port s last-line)
  (if (eof-object? last-line)
      'done
      (let ((next-line (read-line in-port)))
        (if (eof-object? next-line)
            'done
            (begin
              (read-line)
              (show-line last-line)
              (show-line next-line)
              (show-all-lines in-port s (helper in-port s)))))))

(define (page file lines)
  (let ((in-port (open-input-file file)))
    (let ((last-line (helper in-port lines)))
      (begin
        (show-all-lines in-port (- lines 2) last-line)
        (close-input-port in-port)))))


(page "long" 4)
