;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Abs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define (plus num l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ (first l)num)
      (plus num (rest l)))]))

(define(plus5 l)
  (plus 5 l))

(define(add1* l)
  (plus 1 l))

(define(mysub2 l)
  (plus -2 l))

(check-expect(plus5(list 1 2 3 4))(list 6 7 8 9))
(check-expect(add1*(list 1 2 3 4))(list 2 3 4 5))
(check-expect(mysub2(list 1 2 3 4))(list -1 0 1 2))

; Lon Number -> Lon
; select those numbers on l
; that are below t
(define (small l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(< (first l) t)
        (cons (first l)
          (small
            (rest l) t))]
       [else
        (small
          (rest l) t)])]))
	

     	
; Lon Number -> Lon
; select those numbers on l
; that are above t
(define (large l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(> (first l) t)
        (cons (first l)
          (large
            (rest l) t))]
       [else
        (large
          (rest l) t)])]))

#|
(large(list 1 2 3 4 5 6 7 8 9)5)
(small(list 1 2 3 4 5 6 7 8 9)5)
|#

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l)
                   (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

#|
(extract > (list 1 2 3 4 5 6 7 8 9) 5)
(extract < (list 1 2 3 4 5 6 7 8 9) 5)
|#

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

;;(extract squared>? (list 3 4 5) 10)

;; -------------------------------------------------------------

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

(define (absup s l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (s (first l)
            (absup s(rest l)))
         (first l)
         (absup s(rest l)))]))

;; with min and max
(define (absupfast s l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else(s(first l)
           (absupfast s(rest l)))]))

(define(inf-1 l)
  (absup < l))

(define(sup-1 l)
  (absup > l))

(define(inf-2 l)
  (absupfast min l))

(define(sup-2 l)
  (absupfast max l))

#|
(inf-1 (list 1 2 3 4 5))
(sup-1 (list 1 2 3 4 5))

(inf-2 (list 1 2 3 4 5))
(sup-2 (list 1 2 3 4 5))
|#
;; -----------------------------------

; A [Maybe X] is one of: 
; – #false 
; – X

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(define (occurs s los)
  (cond
    ((empty? los)#f)
    (else(if(string=? s(first los))
            (rest los)
            (occurs s(rest los))))))


(check-expect (occurs "a" (list "b" "a" "d" "e"))(list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)


;; --------------------------------------------------------------------


; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
	

  
	
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))


(define (tabulate n something)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (something n)
      (tabulate (sub1 n)something))]))

(define(tab-sqr-from-tabulate n)
  (tabulate n sqr))

(define(tab-tan-from-tabulate n)
  (tabulate n tan))

;;(check-expect(tab-sqrt 5)(tabulate 5 sqrt))
;;(check-expect(tab-sin 5)(tabulate 5 sin))


(define(fold1 l starter proc)
  (cond
    [(empty? l) starter]
    [else
     (proc (first l)
        (fold1 (rest l)starter proc))]))

	
; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))
	

  	
; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))


;; same as fold1
(define(fold2 l starter proc)
  (cond
    [(empty? l) starter]
    [else
     (proc (first l)
        (fold1 (rest l)starter proc))]))

;;(fold2(list (make-posn 10 20) (make-posn 10 30) (make-posn 10 40))emt place-dot)
;;(image*(list (make-posn 10 20) (make-posn 10 30) (make-posn 10 40)))


;; sort-n
;; [[List-of Number] [Number Number-> Boolean] -> [List-of Number]]

;; sort-s
;; [[List-of String] [String String-> Boolean] -> [List-of String]]

;; abstracted
;; [[List-of X] [X X-> Boolean] -> [List-of X]]


;; Exercise 255
;; ,,,

;; map-n
;; [[List-of Number] [Number-> Number] -> [List-of Number]]

;; map-s
;; [[List-of String] [String-> String] -> [List-of String]]

;; abstracted
;; [[List-of X] [X-> X] -> [List-of X]]




; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))
              (foldl / 1 '(6 3 2)))


(define (f*oldl f e l)
  (cond
    ((empty? l) e)
    (else(f*oldl f(f(first l)e)(rest l)))))

(define(add-at-end item lst)
  (cond
    ((empty? lst)(cons item '()))
    (else(cons(first lst)(add-at-end item(rest lst))))))

(define(ret x)x)

(define(build-l*st n fun)
  (cond
    ((<= n 0)'())
    (else(add-at-end(fun(sub1 n))(build-l*st(sub1 n)fun)))))


(check-expect(build-l*st 4 add1)(build-list 4 add1))
(check-expect(build-l*st 4 sub1)(build-list 4 sub1))
(check-expect(build-l*st 4 ret)(build-list 4 ret))



