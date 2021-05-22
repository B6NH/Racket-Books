; Exercise 3

#lang racket

(define (helper vec from index counter)
  (if (= counter -1)
      vec
      (begin
        (vector-set! vec index (vector-ref from counter))
        (helper vec from (- index 1) (- counter 1)))))

(define (copy vec from index size)
  (helper vec from index (- size 1)))

(define (vcapp a b)
  (let ((alen (vector-length a))
        (blen (vector-length b)))
    (let ((len (+ alen blen)))
      (let ((new (make-vector len)))
        (begin
          (copy new b (- len 1) blen)
          (copy new a (- alen 1) alen))))))


(and
  (equal? (vcapp '#(not a) '#(second time)) '#(not a second time))
  (equal? (vcapp '#(rather vague descriptions) '#(algorithm deal))
          '#(rather vague descriptions algorithm deal))
  (equal? (vcapp '#(done with either) '#()) '#(done with either))
  (equal? (vcapp '#() '#(pairs of elements)) '#(pairs of elements)))
