; Exercise 7

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define (myse a b)
  (append (if (word? a) (list a) a)
          (if (word? b) (list b) b)))

(define (myse2 f . restlist)
  (if (empty? restlist)
      f
      (apply myse2
        (cons (myse f (car restlist))
              (cdr restlist)))))

(and
  (equal? (myse '(blackmores night) 'shadow)
          (se '(blackmores night) 'shadow))
  (equal? (myse 'secret 'voyage)
          (se 'secret 'voyage))
  (equal? (myse 'first '(write a version))
          (se 'first '(write a version)))
  (equal? (myse2 '(under a) 'violet '(full moon) 'village '(crystal ball))
          (se '(under a) 'violet '(full moon) 'village '(crystal ball))))
