; Exercise 13

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(greet s)
  (let((fs(first s))(ls(last s)))
    (if(equal? ls 'livingstone)
       (se 'dr ls '(i presume?))
       (se 'hello
           (cond
             ((equal? fs 'queen)'(your majesty))
             ((equal? fs 'bp)(se'(your excellency)ls))
             ((equal? fs 'card)(se'(your eminence)ls))
             (else(se fs 
                      (if(equal? fs 'dr)
                         (if(not(equal? ls 'jr))ls(last(bl s)))
                         '()))))))))

(greet '(john lennon))
(greet '(dr marie curie))
(greet '(dr martin luther king jr))
(greet '(queen elizabeth))
(greet '(david livingstone))
(greet '(bp marek jedraszewski))
(greet '(card konrad krajewski))

