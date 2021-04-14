; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(remdup s)
  (if (empty? s)
      s
      (let ((fst(first s))
            (rst (bf s)))
      (if (member? fst rst)
          (remdup rst)
          (se fst (remdup rst))))))

(equal? (remdup '(ob la di ob la da))
        '(di ob la da))
