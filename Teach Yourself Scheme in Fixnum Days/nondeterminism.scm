(include "helper/functions.scm")

; Nondeterminism

; Variable in which continuation will be stored
(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(define-macro amb

  ; Indefinite number of subexpressions
  (lambda alts...

    ; Store current amb-fail value
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)

          ; Try all subexpressions
          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                         (lambda ()

                           ; Reset amb-fail
                           (set! amb-fail +prev-amb-fail)

                           ; Try next alternate
                           (+fk 'fail)))

                       ; Exit amb call
                       (+sk ,alt))))
                 alts...)

          ; All alternates fail
          (+prev-amb-fail))))))

(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi)
          (amb)
          (amb i (loop (+ i 1)))))))

; Amb fail if pred is false
(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define (distinct? lst)
  (not (eqv? (car lst) (cadr lst))))

(define (xor a b)
  (or (and (not a) b)
      (and a (not b))))

(define (prime? n)
  (letrec
    ((h (lambda (c a)
           (or (= 1 c)
               (and (not (zero? (remainder a c)))
                    (h (- c 1) a))))))
    (h (- n 1) n)))

(define (all-true pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (all-true pred (cdr lst)))))

(define (all-false pred lst)
  (or (null? lst)
      (and (not (pred (car lst)))
           (all-false pred (cdr lst)))))

(define gen-prime
  (lambda (hi)
    (let ((i (number-between 2 hi)))
      (assert (prime? i))
      i)))

(define-macro bag-of
  (lambda (e)
    `(let ((+prev-amb-fail amb-fail)
           (+results '()))
       (if (call/cc
             (lambda (+k)
               (set! amb-fail (lambda () (+k #f)))
               (let ((+v ,e))

                 ; Add next value
                 (set! +results (cons +v +results))
                 (+k #t))))
           (amb-fail))
       (set! amb-fail +prev-amb-fail)
       (reverse! +results))))

(define choose-color
  (lambda ()
    (amb 'red 'yellow 'blue 'white)))

(define line
  (lambda ()
    (display "------------------------------\n")))

; -------------------------------------------------------------

; Kalotan puzzle

(define solve-kalotan-puzzle
  (lambda ()
    (let ((parent1 (amb 'm 'f))
          (parent2 (amb 'm 'f))
          (kibi (amb 'm 'f))
          (kibi-self-desc (amb 'm 'f))
          (kibi-lied? (amb #t #f)))

      ; Heterosexual couple
      (assert
       (distinct? (list parent1 parent2)))

      ; Error?
;;       (assert
;;        (if (eqv? kibi 'm)
;;            (not kibi-lied?)))

      ; Kibi lies
      (assert
       (if kibi-lied?
           (xor
            (and (eqv? kibi-self-desc 'm)
                 (eqv? kibi 'f))
            (and (eqv? kibi-self-desc 'f)
                 (eqv? kibi 'm)))))

      ; Kibi tells the truth
      (assert
       (if (not kibi-lied?)
           (xor
            (and (eqv? kibi-self-desc 'm)
                 (eqv? kibi 'm))
            (and (eqv? kibi-self-desc 'f)
                 (eqv? kibi 'f)))))

      ; Male - parent1, Female - parent2
      (assert
       (if (eqv? parent1 'm)
           (and

            ; One true statement
            ; Kibi said he is a boy
            (eqv? kibi-self-desc 'm)

            ; One true and one false statement
            ; Kibi is a girl and doesn't lie or is a boy and lies
            (xor
             (and (eqv? kibi 'f)
                  (eqv? kibi-lied? #f))
             (and (eqv? kibi 'm)
                  (eqv? kibi-lied? #t))))))

      ; Female - parent1, Male - parent2
      (assert
       (if (eqv? parent1 'f)

           ; Two true statements
           (and
            (eqv? kibi 'f)
            (eqv? kibi-lied? #t))))

      (list parent1 parent2 kibi))))

; -------------------------------------------------------------

; Map coloring

(define color-europe
  (lambda ()

    ; choose colors for each country
    (let ((p (choose-color)) ; Portugal
          (e (choose-color)) ; Spain
          (f (choose-color)) ; France
          (b (choose-color)) ; Belgium
          (h (choose-color)) ; Holland
          (g (choose-color)) ; Germany
          (l (choose-color)) ; Luxemb
          (i (choose-color)) ; Italy
          (s (choose-color)) ; Switz
          (a (choose-color))) ; Austria

      ; construct the adjacency list for
      ; each country: the 1st element is
      ; the name of the country; the 2nd
      ; element is its color; the 3rd
      ; element is the list of its
      ; neighbors’ colors
      (let ((portugal
             (list 'portugal p
                   (list e)))
            (spain
             (list 'spain e
                   (list f p)))
            (france
             (list 'france f
                   (list e i s b g l)))
            (belgium
             (list 'belgium b
                   (list f h l g)))
            (holland
             (list 'holland h
                   (list b g)))
            (germany
             (list 'germany g
                   (list f a s h b l)))
            (luxembourg
             (list 'luxembourg l
                   (list f b g)))
            (italy
             (list 'italy i
                   (list f a s)))
            (switzerland
             (list 'switzerland s
                   (list f i a g)))
            (austria
             (list 'austria a
                   (list i s g))))

        ; Set country list
        (let ((countries
               (list portugal spain
                     france belgium
                     holland germany
                     luxembourg
                     italy switzerland
                     austria)))

          ; the color of a country
          ; should not be the color of
          ; any of its neighbors
          (for-each
           (lambda (c)
             (assert
              (not (memq (cadr c)
                         (caddr c)))))
           countries)

          ; output the color
          ; assignment
          (for-each
           (lambda (c)
             (display (car c))
             (display " - ")
             (display (cadr c))
             (newline))
           countries))))))

; -------------------------------------------------------------

(define prime '(2 3 5 7 11 13 17 19 23 29))
(define composite '(4 6 8 9 10 12 14 15 16 18))

(begin

  (initialize-amb-fail)

  (line)

  (display "Colors of countries:\n")
  (color-europe)

  (line)

  (display
    (and (= (amb 1 2 3 4 5 6 7 8 9 10) 1)
         (all-true prime? prime)
         (all-false prime? composite)
         (equal? (bag-of (gen-prime 30)) prime)
         (equal? (solve-kalotan-puzzle) '(f m f))))

  (newline))

