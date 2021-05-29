; Exercise 12

#lang racket

(define (vector-swap! vector index1 index2)
  (let ((temp (vector-ref vector index1)))
    (vector-set! vector index1 (vector-ref vector index2))
    (vector-set! vector index2 temp)))

(define (smallest-from vec index best)
  (if (= index (vector-length vec))
      best
      (smallest-from
        vec
        (+ index 1)
        (if (< (vector-ref vec index)
               (vector-ref vec best))
            index
            best))))

(define (smallest vec index)
  (smallest-from vec (+ index 1) index))

(define (sesort-from vec index)
  (if (= index (vector-length vec))
      'done
      (begin
        (vector-swap! vec index (smallest vec index))
        (sesort-from vec (+ index 1)))))

(define (sesort vec)
  (sesort-from vec 0))


(define unsorted (vector 23 4 18 7 95 60))

(and
  (equal? (sesort unsorted) 'done)
  (equal? unsorted #(4 7 18 23 60 95)))

