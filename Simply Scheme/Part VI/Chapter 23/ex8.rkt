; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define *lap-vector* (make-vector 100))
(define *winner* #f)

(define (initialize-lap-vector index)
  (if (< index 0)
      'done
      (begin (vector-set! *lap-vector* index 0)
      (initialize-lap-vector (- index 1)))))

(define (lap car-number)
  (vector-set! *lap-vector*
               car-number
               (+ (vector-ref *lap-vector* car-number) 1))
  (let ((lp (vector-ref *lap-vector* car-number)))
    (begin
      (if (and (= lp 4) (not *winner*))
          (begin
            (set! *winner* #t)
            (show-line (se "Car" car-number "wins!")))
          'continue)
      lp)))

(define (find-leader vec max index)
  (if (= -1 index)
      max
      (find-leader vec
                   (if (> (vector-ref vec index) (vector-ref vec max))
                       index
                       max)
                   (- index 1))))

(define (leader)
  (find-leader *lap-vector*
               (- (vector-length *lap-vector*) 1)
               (- (vector-length *lap-vector*) 2)))


(and
  (equal? (initialize-lap-vector 99) 'done)
  (equal? (lap 2) 1)
  (equal? (lap 5) 1)
  (equal? (lap 2) 2)
  (equal? (lap 5) 2)
  (equal? (lap 2) 3)
  (equal? (lap 5) 3)
  (equal? (lap 2) 4)
  (equal? (lap 5) 4)
  (equal? (lap 2) 5)
  (equal? (lap 5) 5)
  (equal? (lap 2) 6)
  (equal? (lap 2) 7)
  (equal? (leader) 2))
