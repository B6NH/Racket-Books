; Exercise 8

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (clones n w)
  (if (= n 0)
      '()
      (se w (clones (- n 1) w))))

(define (expand s)
  (if (empty? s)
      '()
      (let ((fst (first s)) (rst (bf s)))
        (if (number? fst)
            (se (clones fst (first rst)) (expand (bf rst)))
            (se fst (expand rst))))))

(and
  (equal? (expand '(4 calling birds 3 french hens))
          '(calling calling calling calling birds
            french french french hens))
  (equal? (expand '(the 7 samurai))
          '(the samurai samurai samurai samurai
            samurai samurai samurai)))


