; Exercise 11

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (remove-adjacent-duplicates s)
  (if (<= (count s) 1)
      s
      (let ((fst (first s))
            (rst (bf s)))
        (se
          (if (equal? fst (first rst))
              '()
              fst)
          (remove-adjacent-duplicates rst)))))


(and
  (equal? (remove-adjacent-duplicates '(y a b b a d a b b a d o o))
          '(y a b a d a b a d o))
  (equal? (remove-adjacent-duplicates '(yeah yeah yeah))
          '(yeah)))
