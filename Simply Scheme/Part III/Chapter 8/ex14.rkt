; Exercise 14

#lang racket
(require (planet dyoo/simply-scheme:2:2))
  
(define(subword wd start stop)
  ((repeated bf (- start 1))((repeated bl (- (count wd) stop)) wd)))


(and
  (equal?(subword 'polythene 5 8)
                      'then)
  (equal?(subword 'polythene 4 4)
                     'y)
  (equal?(subword 'polythene 1 9)
                  'polythene)
  (equal?(subword 'polythene 1 3)
                  'pol)
  (equal?(subword 'polythene 7 9)
                        'ene))
