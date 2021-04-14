; Exercise 16

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (is-vowel? x)
  (member? x 'aeiou))

(define (drop-vowels w)
  (if (or (empty? w)
          (not (is-vowel? (first w))))
       w
     (drop-vowels (bf w))))


(define (syllables w)
  (if (empty? w)
      0
      (let ((rst (bf w)))
        (if (is-vowel? (first w))
            (+ 1 (syllables (drop-vowels rst)))
            (syllables rst)))))


(and
  (equal? (syllables 'rmzgd) 0)
  (equal? (syllables 'eiur) 1) ; eiu
  (equal? (syllables 'oaiue) 1) ; oaiue
  (equal? (syllables 'miaoued) 1) ; iaoue
  (equal? (syllables 'fire) 2) ; i e
  (equal? (syllables 'soaring) 2) ; oa i
  (equal? (syllables 'eulogia) 3) ; eu o ia
  (equal? (syllables 'meigauozoirea) 4)) ; ei auo oi ea
