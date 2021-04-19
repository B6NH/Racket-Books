; Exercise 1

#lang racket

(and
  (equal? (car '(Rod Chris Colin Hugh Paul)) 'Rod)
  (equal? (cadr '(Rod Chris Colin Hugh Paul)) 'Chris)
  (equal? (cdr '(Rod Chris Colin Hugh Paul))
          '(Chris Colin Hugh Paul))
  (equal? (cons '(Rod Argent) '(Chris White))
          '((Rod Argent) Chris White))
  (equal? (append '(Rod Argent) '(Chris White))
          '(Rod Argent Chris White))
  (equal? (list '(Rod Argent) '(Chris White))
          '((Rod Argent) (Chris White)))
  (equal? (caadr '((Rod Argent) (Chris White)
                  (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
          'Chris)
  (equal?
    (assoc 'Colin '((Rod Argent) (Chris White)
                    (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
    '(Colin Blunstone))
  (equal?
    (assoc 'Argent '((Rod Argent) (Chris White)
                    (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
    #f))

; error
; (car 'Rod)
