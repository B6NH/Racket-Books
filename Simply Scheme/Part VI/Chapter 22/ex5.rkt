; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (nhelper in-port out-port)
  (let ((fline (read-line in-port)))
    (if (eof-object? fline)
        'done
        (begin
          (show-line fline out-port)
          (helper in-port out-port fline)))))

(define (helper in-port out-port last-line)
  (let ((line (read-line in-port)))
    (cond
      ((eof-object? line) 'done)
      ((equal? line last-line)
       (helper in-port out-port last-line))
      (else
        (begin
          (show-line line out-port)
          (helper in-port out-port line))))))

(define (remove-cons in-file out-file)
  (let ((in-port (open-input-file in-file))
        (out-port (open-output-file out-file)))
    (begin
      (nhelper in-port out-port)
      (close-input-port in-port)
      (close-output-port out-port)
      'done)))



(remove-cons "cnsc" "cnsc_out")
