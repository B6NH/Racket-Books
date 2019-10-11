;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TrafficLights) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define GREEN (circle 25 "solid" "green"))
(define RED(circle 25 "solid" "red"))
(define BACKGROUND(empty-scene 100 100))
(define CYCLETIME 20)

(define(render-num num)
  (text(number->string num)50(if(odd? num)"red" "green")))


;; state is
;; "red" or number
;; numbers go from 20 to 0
;; for 20 to 10 state is rendered as green light
;; for 10 to 1 is rendered as number


(define startstate "red")

(define(render state)
  (overlay
   (cond
     ((string? state)RED)
     ((number? state)(if(> state (/ CYCLETIME 2))GREEN(render-num state))))
   BACKGROUND))

(define(control state ke)
  (cond
    ((and(string=? ke "c")
         (string? state)
         (string=? state "red"))
     CYCLETIME)
    (else state)))

(define(update state)
  (cond
    ((string? state)state)
    ((number? state)
     (if(> state 1)
        (- state 1)
        "red"))))


#|
(big-bang startstate
  (on-tick update (/ 1 4))
  (to-draw render)
  (on-key control))
|#

(check-expect(update startstate)startstate)

;; sterowanie - c

