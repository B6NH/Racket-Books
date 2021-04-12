; Bridge

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(card-val card)
  (let ((s(last card)))
    (cond
      ((equal? s 'a) 4)
      ((equal? s 'k) 3)
      ((equal? s 'q) 2)
      ((equal? s 'j) 1)
      (else 0))))
      
(define(high-card-points cards)
  (accumulate + (every card-val cards)))
  
(define(count-suit color cards)
  (count (keep (lambda(x)(equal? color (first x))) cards)))
  
(define(suit-counts cards)
  (every (lambda(x)(count-suit x cards)) '(s h c d)))
  
(define(suit-dist-points nmbr)
  (cond
    ((= nmbr 2) 1)
    ((= nmbr 1) 2)
    ((= nmbr 0) 3)
    (else 0)))

(define(hand-dist-points cards)
  (accumulate +
    (every suit-dist-points (suit-counts cards))))
    
(define(bridge-val cards)
  (+ (high-card-points cards)
     (hand-dist-points cards)))

(and
  (equal? (card-val 'cq) 2)
  (equal? (card-val 's7) 0)
  (equal? (card-val 'ha) 4)
  (equal? (high-card-points '(sa s10 hq ck c4)) 9)
  (equal? (high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 13)
  (equal? (count-suit 's '(sa s10 hq ck c4)) 2)
  (equal? (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 2)
  (equal? (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 5)
  (equal? (suit-counts '(sa s10 hq ck c4)) '(2 1 2 0))
  (equal? (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) '(5 3 2 3))
  (equal? (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) '(5 1 2 5))
  (equal? (suit-dist-points 2) 1)
  (equal? (suit-dist-points 7) 0)
  (equal? (suit-dist-points 0) 3)
  (equal? (hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 1)
  (equal? (hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 3)
  (equal? (bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 14)
  (equal? (bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 8))
