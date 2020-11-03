;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname MaEx) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 28 Mathematical Examples

(define ε 0.01)

;; Exercise 455. Translate this mathematical formula

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

;; nachylenie funkcji f w punkcie r1
(define(slope f r1)
  (* (/ 1(* 2 ε))
     (-(f(+ r1 ε))(f(- r1 ε)))))

;; Exercise 456. Design root-of-tangent

;; root of tangent on function at r1
(define(root-of-tangent f r1)
  (- r1(/(f r1)(slope f r1))))


; [Number -> Number] Number -> Number
; finds a number r such that (f r) is small
; generative repeatedly generates improved guesses
(check-within (newton poly 1) 2 ε)
(check-within (newton poly 3.5) 4 ε)
 
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) ε) r1]
    [else (newton f (root-of-tangent f r1))]))


;; Exercise 457. Design the function double-amount

(define(double-amount initial rate)
  (local
    ((define double(* initial 2))
     (define(h count current)
       (cond
         ((>= current double)count)
         (else(h(add1 count)
                (+ current(* rate current)))))))
    (h 0 initial)))


;; Rule of 72
(check-expect(double-amount 1000 0.03)(/ 72 3))
(check-expect(double-amount 1000 0.04)(/ 72 4))
(check-expect(double-amount 1000 0.06)(/ 72 6))


;; 28.2 Numeric Integration

(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))


(check-within (integrate-kepler constant 12 22) 200 ε)
(check-within (integrate-kepler linear 0 10) 100 ε)
;; tolerance 130
(check-within (integrate-kepler square 0 10)
              1000
              130)

(check-within (integrate-rectangles constant 12 22) 200 ε)
(check-within (integrate-rectangles linear 0 10) 100 ε)
(check-within (integrate-rectangles square 0 10)
              1000
              ε)


(check-within (integrate-dc constant 12 22) 200 ε)
(check-within (integrate-dc linear 0 10) 100 ε)
(check-within (integrate-dc square 0 10)
              1000
              ε)


(check-within (integrate-adaptative constant 12 22) 200 ε)
(check-within (integrate-adaptative linear 0 10) 100 ε)
(check-within (integrate-adaptative square 0 10)
              1000
              ε)


(define(integrate-kepler f l r)
  (local((define(area l r)
           (*(/ 1 2)
             (- r l)
             (+(f l)(f r))))
         (define mid(/(+ l r)2)))
    (+(area l mid)
      (area mid r))))


;; equal formula for kepler area
#|
(define(area f l r)
  (+(*(- r l)(f r))
    (*(/ 1 2)(- r l)(-(f l)(f r)))))
|#

;; Exercise 459. Another simple integration method divides

;; number of rectangles
(define R 1000)

(define(integrate-rectangles f a b)
  (local((define W(/(- b a)R));; rectangle width
         (define S(/ W 2))
         (define(A i);; area of rectangle at f i
           (* W(f(+ a(* i W)S))))
         (define(h count)
           (cond
             ((= count R)0)
             (else(+(A count)
                    (h(add1 count)))))))
    (h 0)))


;; Exercise 460. Develop the algorithm integrate-dc

(define(integrate-dc f a b)
  (if(<=(- b a)0.1)
     (integrate-kepler f a b)
     (local((define mid(/(+ a b)2)))
       (+(integrate-dc f a mid)
         (integrate-dc f mid b)))))
  

;; Exercise 461. Design integrate-adaptive.

(define(integrate-adaptative f a b)
  (local((define mid(/(+ a b)2))
         (define left(integrate-kepler f a mid))
         (define right(integrate-kepler f mid b))
         (define diff(abs(- left right))))
    (if(<= diff(* ε(- b a)))
       (integrate-kepler f a b)
       (+(integrate-adaptative f a mid)
         (integrate-adaptative f mid b)))))
    
    
;; 28.3 Project: Gaussian Elimination

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define M2
  (list(list 2 2 3 10)
       (list   3 9 21)
       (list     1  2)))

(define M3
  (list(list 2 2 3 10)
       (list   3 9 21)
       (list  -3 -8 -19)))

(define M5(list (list 2  3  3 8)
                (list 2  3 -2 3)
                (list 4 -2  2 4)))

(define NOSOLUTION
  (list
   (list 2 2 2 6)
   (list 2 2 4 8)
   (list 2 2 1 2)))
       
  
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))


;; Exercise 462. Design the function check-solution.

(define(is-true x)x)
(define(is-not-true x)(not x))

(check-expect(plug-in(lhs(first M))S)10)
(check-expect(plug-in(lhs(second M))S)31)
(check-expect(plug-in(lhs(third M))S)1)
(check-satisfied(check-solution M S)is-true)
(check-satisfied(check-solution M '(1 1 2))is-true)
(check-satisfied(check-solution M '(1 1 3))is-not-true)

(check-expect(plug-in(lhs(first M2))S)10)
(check-expect(plug-in(lhs(second M2))S)21)
(check-expect(plug-in(lhs(third M2))S)2)
(check-satisfied(check-solution M2 S)is-true)
(check-satisfied(check-solution M2 '(1 1 2))is-true)
(check-satisfied(check-solution M2 '(1 1 3))is-not-true)

(check-expect(plug-in(lhs(first M3))S)10)
(check-expect(plug-in(lhs(second M3))S)21)
(check-expect(plug-in(lhs(third M3))S)-19)
(check-satisfied(check-solution M3 S)is-true)
(check-satisfied(check-solution M3 '(1 1 2))is-true)
(check-satisfied(check-solution M3 '(1 1 3))is-not-true)

(check-satisfied(check-solution(list (list 2  3  3   8)
                                     (list   -8 -4 -12)
                                     (list      -5  -5))'(1 1 1))
                is-true)


(define(plug-in left-side S)
  (local((define(h left-side S)
           (cond
             ((empty? left-side)0)
             (else(+(*(first left-side)(first S))
                    (plug-in(rest left-side)(rest S)))))))
    (if(=(length left-side)(length S))
       (h left-side S)
       (plug-in left-side(rest S)))))
         


(define(check-solution m s)
  (cond
    ((empty? m)#true)
    (else(and(=(plug-in(lhs(first m))s)
               (rhs(first m)))
             (check-solution(rest m)s)))))


;; Exercise 465. Design subtract. The function consumes two

(check-expect(subtract-sub'(2 5 12 31)'(2 2 3 10))'(3 9 21))
(check-expect(subtract-sub'(4 7 15 41)'(2 2 3 10))'(3 9 21))
(check-expect(subtract-sub'(-3 -8 -19)'(3 9 21))'(1 2))
(check-expect(subtract-div'(2 5 12 31)'(2 2 3 10))'(3 9 21))
(check-expect(subtract-div'(4 7 15 41)'(2 2 3 10))'(3 9 21))
(check-expect(subtract-div'(-3 -8 -19)'(3 9 21))'(1 2))
(check-expect(sub'(2 5 12 31)'(2 2 3 10))'(0 3 9 21))
(check-expect(add'(-3 -8 -19)'(3 9 21))'(0 1 2))

(define(gen eq1 eq2 sym)
  (cond
    ((empty? eq1)'())
    (else(cons(sym(first eq1)(first eq2))
              (gen(rest eq1)(rest eq2)sym)))))

(define(sub eq1 eq2)
  (gen eq1 eq2 -))


(define(add eq1 eq2)
  (gen eq1 eq2 +))


;; with subtraction
(define(subtract-sub eq1 eq2)
  (cond
    ((zero?(first eq1))(rest eq1))
    (else(subtract-sub((if(<(first eq1)0)add sub) eq1 eq2)eq2))))


;; with division
(define(subtract-div eq1 eq2)
  (local((define factor(/(first eq1)(first eq2)))
         (define(h eq1 eq2)
           (cond
             ((empty? eq1)'())
             (else(cons(-(first eq1)
                         (*(first eq2)factor))
                       (h(rest eq1)(rest eq2)))))))
    (h(rest eq1)(rest eq2))))



(check-expect
 (triangulate-sub M)
 (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))
(check-expect
 (triangulate-sub (list (list 2 2 3 10) (list 2 5 12 31) (list 4 1 -2 1)))
 (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))

(check-expect
 (triangulate-div M)
 (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))
(check-expect
 (triangulate-div (list (list 2 2 3 10) (list 2 5 12 31) (list 4 1 -2 1)))
 (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))

(check-expect
 (triangulate-revised M)
 (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))
(check-expect
 (triangulate-revised (list (list 2 2 3 10) (list 2 5 12 31) (list 4 1 -2 1)))
 (list (list 2 2 3 10) (list 3 9 21) (list 1 2)))
(check-expect
 (triangulate-revised M5)
 (list (list 2  3  3   8)
       (list   -8 -4 -12)
       (list      -5  -5)))
                     
(check-error
 (triangulate-div M5
                 "/: division by zero"))
 

(define(last x)
  (cond
    ((empty? x)(error "list is empty"))
    ((empty?(rest x))(first x))
    (else(last(rest x)))))

(check-error(last'())"list is empty")
(check-expect(last'(4 5))5)


(define(triangulate-sub M)
  (cond
    ((=(length(last M))2)M)
    (else(local((define top(first M))
                (define rst(rest M))
                (define(h lst)
                  (cond
                    ((empty? lst)'())
                    (else(cons(subtract-sub(first lst)top)
                              (h(rest lst)))))))
           (cons top(triangulate-sub(h rst)))))))


(define(triangulate-div M)
  (cond
    ((=(length(last M))2)M)
    (else(local((define top(first M))
                (define rst(rest M))
                (define(h lst)
                  (cond
                    ((empty? lst)'())
                    (else(cons(subtract-div(first lst)top)
                              (h(rest lst)))))))
           (cons top(triangulate-div(h rst)))))))
           
    
;; Exercise 467. Revise the algorithm triangulate

(check-expect(rotate '(5 6 7 8))'(6 7 8 5))

(define(rotate L)
  (append (rest L) (list (first L))))

;; most advanced triangulate version
(define(triangulate-revised M)
  (cond
    ((=(length(last M))2)M)
    ((zero?(first(first M)))
     (if(not(andmap(lambda(x)(zero?(first x)))M))
        (triangulate-revised(rotate M))
        (error "cant rotate! all equations start with 0!")))
    (else(local((define top(first M))
                (define rst(rest M))
                (define(h lst)
                  (cond
                    ((empty? lst)'())
                    (else(cons(subtract-div(first lst)top)
                              (h(rest lst)))))))
           (cons top(triangulate-revised(h rst)))))))


(check-error
 (triangulate-revised NOSOLUTION)
 "cant rotate! all equations start with 0!")


;; Exercise 469. Design the solve function.


(check-expect(solve-equation'(3 9)'())'(3))
(check-expect(solve-equation'(3 9 21)'(2))'(1 2))
(check-expect(solve-equation'(2 2 3 10)'(1 2))'(1 1 2))
(check-expect(solve(triangulate-revised M))S)

(define(solve-equation eq sol)
  (cons(/(-(rhs eq)(plug-in(rest(lhs eq))sol))(first eq))sol))

#|
(define(solve m)
  (local((define reversed(reverse m))
         (define(h m sol)
           (cond
             ((empty? m)sol)
             (else(h(rest m)(solve-equation(first m)sol))))))
    (h reversed '())))
|#

(define(solve m)
  (foldr(lambda(x y)(solve-equation x y)) '() m))


;; Exercise 470. Define gauss

(check-expect(gauss M)'(1 1 2))
(check-expect(gauss M5)'(1 1 1))
(check-expect(gauss(list(list 1 1 1 12)
                        (list 1 1 -1 2)
                        (list 1 -1 1 4)))
             '(3 4 5))
(check-expect(gauss(list(list 3 1 1 -2)
                        (list 2 2 3 8)
                        (list 1 3 2 6)))
             '(-2 0 4))
(check-expect(gauss(list(list 1 1 1 0)
                        (list 1 2 3 -3)
                        (list 1 1 -1 4)))
             '(1 1 -2))
(check-expect(gauss(list(list 2 -1 1 -4)
                        (list 8 2 -5 -10)
                        (list 4 1 1 2)))
             '(-1 4 2))
(check-expect(gauss(list(list 1 1 0 13)
                        (list 1 0 -1 5)
                        (list 0 1 -1 2)))
             '(8 5 3))
(check-error(gauss NOSOLUTION)"cant rotate! all equations start with 0!")


(define(gauss m)
  (solve(triangulate-revised m)))

