
; Enter Scheme
; Gambit: gsi enter.scm

(define def 14)

(define string1 "Hello, World!")
(define string2 (string #\A #\l #\b #\e #\x #\o))

(define vector1 (vector 0 1 2 3 4 5))
(define vector2 (make-vector 4))

(define dpair1 (cons 5 #t)) ; (5 . #t)
(define dpair2 '(#t . 7))
(define dpair3 (cons (cons 1 2) 3))
(define dpair4 (cons 1 (cons 2 (cons 3 (cons 4 '())))))

(begin
  (set! def #\m) (string-set! string2 4 #\d) (vector-set! vector1 2 15)
  (display string1) (newline) (display string2) (newline)
  (display
    (and
      (boolean? #t) (not (not "Ha ha")) (number? 15)
      (complex? 2+3i) (real? 3.14) (rational? 15/8)
      (integer? 26) (integer? #b1000101) (not (eqv? 42 42.0))
      (= 5 5) (>= 20 12) (= (expt 2 6) 64) (= (/ 4) (/ 1 4))
      (= (max 4 1 7 3 2) 7) (char? #\c) (char? #\newline)
      (char<? #\a #\g) (char-ci=? #\a #\A) (symbol? 'def)
      (not (eqv? 'Calorie 'calorie)) (eqv? def #\m)
      (eqv? (string-ref string2 3) #\e)
      (string=? (string-append "abc" "def") "abcdef")
      (vector? vector2) (equal? vector1 (vector 0 1 15 3 4 5))
      (= (car dpair1) 5) (= (cdr dpair2) 7) (= (cdar dpair3) 2)
      (equal? dpair4 (list 1 2 3 4)) (= (list-ref dpair4 2) 3)
      (equal? (list-tail dpair4 2) '(3 4)) (null? '())
      (equal? (integer->char 65) #\A) (equal? (string->symbol "string") 'string)))
  (newline)
  (display "Bye!" (current-output-port))
  (newline))

