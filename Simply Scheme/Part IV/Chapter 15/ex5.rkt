; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (prepend-every n w)
  (if (empty? w)
      '()
      (se (word n (first w))
          (prepend-every n (bf w)))))

(define (prepend-all a b)
  (if (empty? a)
      '()
      (se (prepend-every (first a) b)
          (prepend-all (bf a) b))))

(define (letters-for-number n)
  (cond
    ((= n 2) '(a b c))
    ((= n 3) '(d e f))
    ((= n 4) '(g h i))
    ((= n 5) '(j k l))
    ((= n 6) '(m n o))
    ((= n 7) '(p q r s))
    ((= n 8) '(t u v))
    ((= n 9) '(w x y z))))


(define (phone-spell n)
  (if (empty? n)
      '("")
      (prepend-all (letters-for-number (first n))
                   (phone-spell (bf n)))))

(and
  (equal? (phone-spell 2) '(a b c))
  (equal? (phone-spell 25) '(aj ak al bj bk bl cj ck cl))
  (equal? (phone-spell 569)
    '(jmw jmx jmy jmz jnw jnx jny jnz jow jox joy joz
      kmw kmx kmy kmz knw knx kny knz kow kox koy koz
      lmw lmx lmy lmz lnw lnx lny lnz low lox loy loz)))
