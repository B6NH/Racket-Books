; Exercise 6

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (helper s)
  (let ((rst (bf s)))
    (if (empty? (bf rst))
        s
        (se (helper (bl (bf rst)))
            'that
            (last s)
            (first s)
            (first rst)))))

(define (unscramble s)
  (let ((rst (bf s)))
    (se (first s)
        (first rst)
        (helper (bf rst)))))


(and
  (equal?
    (unscramble '(this is the roach the gladiator killed))
    '(this is the gladiator that killed the roach))
  (equal?
    (unscramble
      '(this is the rat the cat the dog
        the boy the girl saw owned chased bit))
    '(this is the girl that saw the boy that
      owned the dog that chased the cat that bit the rat)))
