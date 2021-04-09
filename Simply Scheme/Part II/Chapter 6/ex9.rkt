; Exercise 9

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define(thismany n s)
  (let((pl(lambda(x)
            (if(equal?(last s)'y)
               (word(bl s)'ies)
               (word s 's)))))
  (se n(if(= n 1)s(pl s)))))


(and
  (equal? (thismany 1 'partridge)'(1 partridge))
  (equal? (thismany 2 'partridge)'(2 partridges))
  (equal? (thismany 1 'french-hen)'(1 french-hen))
  (equal? (thismany 3 'french-hen)'(3 french-hens))
  (equal? (thismany 1 'butterfly)'(1 butterfly))
  (equal? (thismany 5 'butterfly)'(5 butterflies)))

