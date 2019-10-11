;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Playg) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))

(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))

(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))


(define (random-posns/bad n)
  (build-list
    3
    (lambda (i)
      (make-posn 6 6))))

(define(n-inside-playground? x)
  (lambda(res)
    (and(=(length res)x)
        (andmap(lambda(g)
                 (and(>(posn-x g)0)
                     (<(posn-x g)WIDTH)
                     (>(posn-y g)0)
                     (<(posn-y g)HEIGHT)))res))))