; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (get-list-from lst pos)
  (if (zero? pos)
      lst
      (get-list-from (cdr lst) (- pos 1))))

(define (check-lines port1 port2 line1 line2 pos1 pos2 out)
  (if (or (eof-object? line1) (eof-object? line2))
      'done
      (let ((key1 (list-ref line1 pos1))
            (key2 (list-ref line2 pos2)))
        (cond
          ((equal? key1 key2)
           (begin
             (show (append line1 (get-list-from line2 (+ pos2 1))) out)
             (check-lines port1 port2 (read port1) (read port2) pos1 pos2 out)))
          ((before? key2 key1)
           (check-lines port1 port2 line1 (read port2) pos1 pos2 out))
          (else
            (check-lines port1 port2 (read port1) line2 pos1 pos2 out))))))

(define (join roster grades pos1 pos2 out)
  (let ((roster-port (open-input-file roster))
        (grades-port (open-input-file grades))
        (out-port (open-output-file out)))
    (begin
      (check-lines roster-port grades-port (read roster-port) (read grades-port)
                   (- pos1 1) (- pos2 1) out-port)
      (close-input-port grades-port)
      (close-input-port roster-port)
      (close-output-port out-port))))


(equal? (get-list-from '(5 7 10 12 15) 2) '(10 12 15))

(join "class-roster" "grades" 3 1 "combined-file")
