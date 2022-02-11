
; Forms & Conditionals

(define add3 (lambda (x) (+ x 3)))
(define area
  (lambda (length breadth)
    (* length breadth)))
(define manyArgs
  (lambda (a b . c) c))
(define someArgs '(5 6 7))
(define implicitBegin
  (lambda (x y z)
    (display z) (display y) (display x) (newline) 8))
(define ifTest
  (lambda (x) (if (> x 50) 'safe 'unsafe)))
(define noElse
  (lambda (x) (if (> x 20) 'good)))
(define implicitBeginWhen
  (lambda (x)
    (when (> x 10)
      (display 4)
      (display 5)
      (display 6)
      (newline)
      1)))
(define implicitBeginUnless
  (lambda (x)
    (unless (> x 50)
      (display 7)
      (display 8)
      (display 9)
      (newline)
      2)))
(define condForm
  (lambda (x)
    (cond
      ((< x 5) 'small)
      ((= x 6) 'six)
      (else 'else))))
(define caseForm
  (lambda (x)
    (case x
      ((7) 'seven)
      ((8) 'eight)
      ((9) 'nine))))
(define andOrForm
  (lambda (x y)
    (or (= x y)
        (and (= x 10) (= y 15)))))

(begin
  (display
    (and
      (= ((lambda (x) (+ x 2)) 5) 7)
      (= (add3 8) 11)
      (= (area 7 8) 56)
      (equal? (manyArgs 5 6 7 8 9) '(7 8 9))
      (equal? (apply * someArgs) 210)
      (equal? (apply * 2 3 someArgs) (* 2 3 5 6 7))
      (= (implicitBegin 3 2 1) 8)
      (eqv? (ifTest 55) 'safe)
      (eqv? (noElse 25) 'good)
      (= (implicitBeginWhen 15) 1)
      (= (implicitBeginUnless 30) 2)
      (equal? (condForm 6) 'six)
      (equal? (caseForm 8) 'eight)
      (andOrForm 10 15)))
  (newline))

