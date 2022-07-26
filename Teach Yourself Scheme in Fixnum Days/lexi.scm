(include "helper/functions.scm")
(include "helper/macros.scm")

; Lexical Variables & Recursion

(random-source-randomize! default-random-source)

(define add2
  (lambda (x)
    (set! x (+ x 2))
    x))

(define incC
  (lambda ()
    (set! c (+ c 1))
    c))

(define createList
  (lambda ()
    (let ((x 5) (y 6) (z 7))
      (list x y z))))

(define letStar
  (lambda ()
    (let* ((x 5) (y (* 2 x)) (z (* 3 y)))
      (list x y z))))

(define procInside
  (lambda ()
    (let ((cons (lambda (x y) (* x y))))
      (cons 9 8))))

(define globalEffect
  (lambda ()
    (let ((c 99))
      (display (incC)) (newline)
      (display (incC)) (newline)
      (display (incC)) (newline))))

(define localEffect
  (lambda ()
    (fluid-let ((c 200)) ; temporarily set global c to 200
      (display (incC)) (newline)
      (display (incC)) (newline)
      (display (incC)) (newline))))

(define throw-die
  (lambda ()
    (ceiling (* (random-real) 6))))

(define throw-two-dice
  (lambda ()
    (+ (throw-die) (throw-die))))

(define is-even?
  (lambda (n)
    (if (= n 0)
        #t
        (is-odd? (- n 1)))))

(define is-odd?
  (lambda (n)
    (if (= n 0)
        #f
        (is-even? (- n 1)))))

(define letRecOddEven
  (lambda ()
    (letrec ((local-even?
              (lambda (n)
                (if (= n 0) #t (local-odd? (- n 1)))))
             (local-odd?
               (lambda (n)
                 (if (= n 0) #f (local-even? (- n 1))))))
      (list (local-even? 23) (local-odd? 23)))))

(define letRecLoop
  (lambda ()
    (letrec
      ((countdown
         (lambda (i)
           (if (= i 0)
               'liftoff
               (begin
                 (display i)
                 (newline)
                 (countdown (- i 1)))))))
      (countdown 3))))

(define namedLet
  (lambda ()
    (let countdown ((i 3)) ; initialization
      (if (= i 0)
          'liftoff
          (begin
            (display i)
            (newline)
            (countdown (- i 1)))))))

(define forE
  (lambda ()
    (for-each
      (lambda (y z) (display (+ y z)))
      '(1 2 3)
      '(4 5 6))))

(define x 5)
(define c 0)

(define *num-trials* 20000)

(begin
  (set! x 10)
  (incC)(incC)
  (globalEffect)
  (localEffect)
  (display (string-append "Random: "
    (number->string (random-integer 100))))
  (newline)
  (display (string-append "Die: "
    (number->string (throw-die))))
  (newline)
  (letRecLoop)
  (namedLet)
  (for-each display (list "One " "Two " "Three\n"))
  (forE)
  (newline)
  (display (monte-carlo throw-die))
  (newline)
  (display (monte-carlo throw-two-dice))
  (newline)
  (display
    (and (= x 10)
         (= (add2 x) 12)
         (= c 5)
         (equal? (createList) '(5 6 7))
         (equal? (letStar) '(5 10 30))
         (= (procInside) 72)
         (is-even? 512)
         (is-odd? 301)
         (equal? (letRecOddEven) '(#f #t))
         (= (list-position 5 '(4 7 1 5 3 2)) 3)
         (not (list-position 1 '(8 2 5)))
         (equal? (reverse! '(1 2 3)) '(3 2 1))
         (equal? (map add2 '(4 5 6)) '(6 7 8))
         (equal? (map + '(1 2 3) '(10 20 30)) '(11 22 33))))
  (newline))

