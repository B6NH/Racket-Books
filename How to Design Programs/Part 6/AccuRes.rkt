;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AccuRes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; 33.3 Accumulators as Results

(define(too-small? a b c)
  (<(distance a b)10))


(define(mid-point a b)
  (make-posn
   (*(/ 1 2)(+(posn-x a)(posn-x b)))
   (*(/ 1 2)(+(posn-y a)(posn-y b)))))



(define(distance a b)
  (sqrt(+(expt(-(posn-x a)(posn-x b))2)
         (expt(-(posn-y a)(posn-y b))2))))
  
  
(check-expect
 (mid-point(make-posn 0 0)(make-posn 10 10))
 (make-posn 5 5))

(check-expect(distance(make-posn 0 0)(make-posn 0 5))5)
(check-expect(distance(make-posn 0 0)(make-posn 5 0))5) 
(check-expect(too-small?(make-posn 0 0)(make-posn 4 0)(make-posn 2 3))#true)
(check-expect(too-small?(make-posn 0 0)(make-posn 15 0)(make-posn 7 10))#false)


(define(add-triangle scene a b c)
  (local((define ax(posn-x a))
         (define ay(posn-y a))
         (define bx(posn-x b))
         (define by(posn-y b))
         (define cx(posn-x c))
         (define cy(posn-y c)))
    (scene+line
     (scene+line
      (scene+line scene ax ay bx by "red")
      bx by cx cy"red")
     cx cy ax ay"red")))

#|
(add-triangle(empty-scene 100 100)
             (make-posn 10 10)
             (make-posn 40 50)
             (make-posn 90 50))
|#

(check-expect
 (add-triangle(empty-scene 100 100)
              (make-posn 10 10)
              (make-posn 40 50)
              (make-posn 90 50))
 (scene+line
  (scene+line
   (scene+line (empty-scene 100 100) 10 10 40 50 "red")
   40 50 90 50 "red")
  90 50 10 10 "red"))


(check-expect
 (add-triangle(empty-scene 100 100)
              (make-posn 10 10)
              (make-posn 40 50)
              (make-posn 90 50))
 (scene+line
  (scene+line
   (scene+line (empty-scene 100 100) 40 50 90 50 "red")
   90 50 10 10 "red")
  10 10 40 50 "red"))


; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to s, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))


(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))
             

;; (add-sierpinski MT A B C)

;; Exercise 526. To compute the endpoints of an equilateral


(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels 
 
; Number -> Posn
; determines the point on the circle with CENTER 
; and RADIUS whose angle is 
 
; examples
; what are the x and y coordinates of the desired 
; point, when given: 120/360, 240/360, 360/360
 
(define (circle-pt factor)
  (local((define radians(d-to-r factor)))
    (make-posn
     (+(*(cos radians)RADIUS)(posn-x CENTER))
     (+(*(sin radians)RADIUS)(posn-y CENTER)))))

(define(circle-pt.v2 factor)
  (local((define polar(make-polar RADIUS(d-to-r factor))))
    (make-posn(+(real-part polar)(posn-x CENTER))
              (+(imag-part polar)(posn-y CENTER)))))


(define(d-to-r degrees)
  (/ degrees(/ 180 pi)))

#|
(overlay(circle RADIUS "outline" "green")
        (add-sierpinski(empty-scene 400 400)
                       (circle-pt 120)
                       (circle-pt 240)
                       (circle-pt 360)))


(overlay(circle RADIUS "outline" "green")
        (add-sierpinski(empty-scene 400 400)
                       (circle-pt.v2 120)
                       (circle-pt.v2 240)
                       (circle-pt.v2 360)))

|#


;; Exercise 527. Take a look at the following two images

(define angle1 -20)
(define angle2 25)


(define (add-savannah scene0 x y length angle)
  (cond
    ((< length 10)scene0)
    (else
     (local((define line-start(make-posn x y))
            (define polar(make-polar length(d-to-r angle)))
            (define line-end(make-posn(+ x(real-part polar))(+ y(imag-part polar))))
            (define midline(mid-point line-start line-end))
            (define d-point(mid-point line-start midline))
            (define u-point(mid-point midline line-end)))
       (add-savannah
        (add-savannah
         (scene+line scene0 x y (posn-x line-end) (posn-y line-end) "red")
         (posn-x d-point)(posn-y d-point)(/ length 1.3)(+ angle angle1))
        (posn-x u-point)(posn-y u-point)(/ length 1.3)(+ angle angle2))))))
                 
                
     
;;(add-savannah(empty-scene 310 300)120 300 110 270)


;; Exercise 528. Graphics programmers often need

(define(bezier image x0 y0 x1 y1 x2 y2)
  (local((define A(make-posn x0 y0))
         (define B(make-posn x1 y1))
         (define C(make-posn x2 y2))
         (define left-mid(mid-point A B))
         (define right-mid(mid-point B C))
         (define mid-center(mid-point left-mid right-mid)))
    (cond
      ((< (distance A B)5)
       (scene+line
        (scene+line
         image(posn-x A)(posn-y A)(posn-x B)(posn-y B)"red")
        (posn-x B)(posn-y B)(posn-x C)(posn-y C)"red"))
      (else
       (bezier
        (bezier
         image
         (posn-x A)(posn-y A)(posn-x left-mid)(posn-y left-mid)(posn-x mid-center)(posn-y mid-center))
        (posn-x mid-center)(posn-y mid-center)(posn-x right-mid)(posn-y right-mid)(posn-x C)(posn-y C))))))

;; (bezier(empty-scene 600 400)80 30 200 650 550 50)


