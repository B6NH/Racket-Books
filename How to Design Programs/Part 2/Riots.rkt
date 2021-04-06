;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Riots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;---------------------------------------------------------------------
;; STATIC RIOT

(define(col n img)
  (cond
    ((zero? n)(empty-scene 0 0))
    (else(above img(col(sub1 n)img)))))

(define(row n img)
  (cond
    ((zero? n)(empty-scene 0 0))
    (else(beside img(row(sub1 n)img)))))



(define ROW(row 8 (rectangle 10 10 "outline" "black")))
(define HALL(overlay(col 18 ROW)(empty-scene 80 180)))
(define REDDOT(circle 3 "solid" "red"))

(define(add-balloons lst)
  (cond
    ((empty? lst)HALL)
    (else(place-image REDDOT
                      (*(posn-x(first lst))10)
                      (*(posn-y(first lst))10)
                      (add-balloons(rest lst))))))
                      

#|
(add-balloons
 (list(make-posn 1 2)
      (make-posn 2 3)
      (make-posn 3 4)
      (make-posn 4 5)))
|#



;;---------------------------------------------------------------------
;; SHOOTING

(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "green"))
(define SHOT (triangle 8 "solid" "red"))


; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
 


; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else 
     (if(<(sub1(first w))0)
        (tock(rest w))
        (cons (sub1 (first w)) (tock (rest w))))]))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
 
; ShotWorld -> Image 
; adds each shotExercise 159 y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

;;(main '())


;;---------------------------------------------------------------------
;; DYNAMIC RIOT


(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob


(define(to-image2 a-pair)
  (add-balloons(pair-lob a-pair)))

(define(tock2 a-pair)
  (cond
    ((zero?(pair-balloon# a-pair))a-pair)
    (else(make-pair(sub1(pair-balloon# a-pair))
                   (cons(make-posn(random 9)(random 19))
                        (pair-lob a-pair))))))

(define (main2 w0)
  (big-bang w0
    [on-tick tock2 0.5]
    [to-draw to-image2]))

(check-expect
 (to-image2 (make-pair 50 (list(make-posn 2 5))))
 (add-balloons(list(make-posn 2 5))))

;;(main2(make-pair 50 '()))




