;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname NatNum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Intermezzo 4: The Nature of Numbers

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))

(define (inex->number an-inex)
  (*(inex-mantissa an-inex)
    (expt
     10(*(inex-sign an-inex)(inex-exponent an-inex)))))


(check-error(create-inex 120 1 1))
(check-expect
 (create-inex 50 -1 20)
 (make-inex 50 -1 20))
(check-expect
 (create-inex 5 -1 19)
 (make-inex 5 -1 19))

(check-expect
 (inex->number(create-inex 12 1 2))
 (* 12(expt 10(* 1 2))))

(check-expect
 (inex->number(create-inex 12 -1 2))
 (* 12(expt 10(* -1 2))))


(check-expect
 (=(inex->number(create-inex 5 -1 19))
   (inex->number(create-inex 50 -1 20)))
 #true)

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

;; Exercise 412. Design inex+.

(check-expect
 (inex+(create-inex 1 1 0)(create-inex 2 1 0))
 (create-inex 3 1 0))

(check-expect
 (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
 (create-inex 11 1 1))

(check-expect
 (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
 (create-inex 11 1 1))

(check-error
 (inex+ MAX-POSITIVE MAX-POSITIVE)
 "addition error")


(define(inex+ ine1 ine2)
  (local
    ((define expo-1(inex-exponent ine1))
     (define expo-2(inex-exponent ine2))
     (define p-ine1
       (if(> expo-1 expo-2)
          (create-inex(* 10 expo-1)
                      (inex-sign ine1)
                      (sub1 expo-1))
          ine1))
     (define p-ine2
       (if(> expo-2 expo-1)
          (create-inex(* 10 expo-2)
                      (inex-sign ine2)
                      (sub1 expo-2))
          ine2))
     (define mantissa
       (+(inex-mantissa p-ine1)
         (inex-mantissa p-ine2)))
     (define exponent(max expo-1 expo-2))
     (define sign(*(inex-sign p-ine1)(inex-sign p-ine2))))
    (if(> mantissa 99)
       (local((define rounded-mantissa(round(/ mantissa 10)))
              (define incremented-exponent(add1 exponent)))
         (if(or(> rounded-mantissa 99)
               (> incremented-exponent 99))
            (error "addition error")
            (create-inex rounded-mantissa
                         sign
                         incremented-exponent)))
       (create-inex mantissa
                    sign
                    exponent))))


     
;; Exercise 413. Design inex*.

(check-expect
 (inex* (create-inex 2 1 4) (create-inex 8 1 10))
 (create-inex 16 1 14))

(check-expect
 (inex* (create-inex 20 1 1) (create-inex  5 1 4))
 (create-inex 10 1 6))

(check-error
 (inex* MAX-POSITIVE MAX-POSITIVE)
 "multiplication error")

(check-error
 (inex* (create-inex 1 -1 10) (create-inex 1 -1 99))
 "multiplication error")



(define(inex* ine1 ine2)
  (local
    ((define WRONG "multiplication error")
     (define mantissa
       (*(inex-mantissa ine1)(inex-mantissa ine2)))
     (define raw-exponent(abs(+(* (inex-sign ine1) (inex-exponent ine1))(* (inex-sign ine2) (inex-exponent ine2)))))
     (define exponent(if(> raw-exponent 99)(error WRONG)raw-exponent))
     (define sign(max(inex-sign ine1)(inex-sign ine2))))
    (if(> mantissa 99)
       (local((define rounded-mantissa(round(/ mantissa 10)))
              (define incremented-exponent(add1 exponent)))
         (if(or(> rounded-mantissa 99)
               (> incremented-exponent 99))
            (error WRONG)
            (create-inex rounded-mantissa
                         sign
                         incremented-exponent)))
       (create-inex mantissa
                    sign
                    exponent))))


(check-expect
 (inex* (create-inex 27 -1 1) (create-inex  7 1 4))
 (create-inex 19 1 4))

(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
 (create-inex 11 -1 1))

;; Exercise 414. As this section illustrates

(define(add n)
  (if(zero? n)
     n
     (+ #i1/185(add(sub1 n)))))

(define(sub n)
  (local
    ((define(helper current times)
       (local((define new-current(- current #i1/185)))
         (if(< new-current 0)
            times
            (helper new-current(add1 times))))))
    (helper n 0)))


;; Exercise 415. ISL+ uses +inf.0 to deal with overflow
(define(abstracted fun)
  (local
    ((define(find n)
       (if(=(expt #i10.0(fun n)) 
            (expt #i10.0(fun(fun n))))
          n
          (find(fun n)))))
    (find 0)))
    
;; Determine the integer n such that (expt #i10.0 n) is an inexact number
;; while (expt #i10. (+ n 1)) is approximated with +inf.0.
(define largest-integer
  (abstracted add1))
;; Determine the smallest integer n such that (expt #i10.0 n) is still
;; an inexact ISL+ number and (expt #i10. (- n 1)) is approximated with 0.
(define smallest-integer
  (abstracted sub1))

(check-expect largest-integer 308)
(check-expect smallest-integer -323)

;; Exercise 418. Design my-expt without using expt.

(check-expect(my-expt 2 3)(expt 2 3))
(check-expect(my-expt 2 0)(expt 2 0))
(check-expect(my-expt 5 7)(expt 5 7))
(check-expect(my-expt 0 4)(expt 0 4))

(define(my-expt a b)
  (cond
    ((zero? b)1)
    (else(* a(my-expt a(sub1 b))))))

(define minex (+ 1 #i1e-12))
(define mexac (+ 1 1e-12))

;; Exercise 419. When you add two inexact numbers of vastly

(define(sum lst)
  (foldl(lambda(x y)(+ x y))0 lst))

(check-expect(sum'(6 3 4 2 1))16)


(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        #i4.2344294738446e+170
        1.0
        #i-8e+269
        0.0
        99.0))

#|
(sum JANUS)
(sum (reverse JANUS))
(sum (sort JANUS <))
|#

;; Exercise 420. JANUS is just a fixed list, but take a look at this function:

(define (oscillate n)
  (local ((define (O i)
            (cond
              [(> i n) '()]
              [else
               (cons (expt #i-0.99 i) (O (+ i 1)))])))
    (O 1)))

(check-within
 (sum (oscillate #i1000.0))
 (sum (reverse (oscillate #i1000.0)))
 0.00000000000001)


(define difference
  (- (* 1e+16 (sum (oscillate #i1000.0)))
     (* 1e+16 (sum (reverse (oscillate #i1000.0))))))

(check-within difference 14 0.0001)


