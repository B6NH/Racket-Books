;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname UfoTank) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define BACKGROUND(empty-scene 200 200))
(define UFO(overlay(circle 3 "solid" "red")(rectangle 13 7 "solid" "green")))

(define TANK(rectangle 35 20 "solid" "brown"))

(define MISSILE(circle 3 "solid" "purple"))

(define HEIGHT(image-height BACKGROUND))

(define TANK-HEIGHT(image-height TANK))

(define DISTANCE 10)


(define-struct tank [loc vel])


(define-struct sigs(ufo tank missile))

(define(tank-render t im)
  (place-image TANK
               (tank-loc t)
               (-(image-height BACKGROUND)
                 (/(image-height TANK)2))
               im))


(define(ufo-render u im)
  (place-image UFO(posn-x u)(posn-y u)im))

(define(missile-render m im)
  (cond
    ((boolean? m) im)
    ((posn? m)(place-image MISSILE(posn-x m)(posn-y m)im))))

(define(si-render s)
  (missile-render
   (sigs-missile s)
   (ufo-render(sigs-ufo s)
              (tank-render(sigs-tank s)
                          BACKGROUND))))

(define(si-game-over? s)
  (or(>=(posn-y(sigs-ufo s))HEIGHT)
     (and(not(boolean?(sigs-missile s)))
         (<(abs(-(posn-x(sigs-missile s))
                 (posn-x(sigs-ufo s))))DISTANCE)
         (<(abs(-(posn-y(sigs-missile s))
                 (posn-y(sigs-ufo s))))DISTANCE))))


(define startstate
  (make-sigs
   (make-posn 100 10);; ufo
   (make-tank 10 1);; tank
   #false));; bomb

                   
;; last image, s is last state of game
(define (si-render-final s) (overlay(rectangle 150 150 "solid" "red")BACKGROUND))



(define(si-move s)
  (make-sigs
   (make-posn(if(=(modulo(posn-y(sigs-ufo s))30)0)
                (+(posn-x(sigs-ufo s))(-(random 60)30))
                (posn-x(sigs-ufo s)))
             (+(posn-y(sigs-ufo s))1))
   (make-tank(+(tank-loc(sigs-tank s))
               (tank-vel(sigs-tank s)))
             (tank-vel(sigs-tank s)))
   (if(posn?(sigs-missile s))
      (make-posn(posn-x(sigs-missile s))
                (-(posn-y(sigs-missile s))2))
      #false)))


(define(si-control s ke)
  (make-sigs
   (make-posn(posn-x(sigs-ufo s))
             (posn-y(sigs-ufo s)))
   (cond
     ((string=? ke "left")
      (make-tank(tank-loc(sigs-tank s))
                (if(>(tank-vel(sigs-tank s))0)
                   (-(tank-vel(sigs-tank s)))
                   (tank-vel(sigs-tank s)))))
     ((string=? ke "right")
      (make-tank(tank-loc(sigs-tank s))
                (if(<(tank-vel(sigs-tank s))0)
                   (* -1(tank-vel(sigs-tank s)))
                   (tank-vel(sigs-tank s)))))
     (else(make-tank(tank-loc(sigs-tank s))
                    (tank-vel(sigs-tank s)))))
   (if(and(string=? ke " ")(boolean?(sigs-missile s)))
      (make-posn(tank-loc(sigs-tank s))170)
      (sigs-missile s))))
      
#|               
(big-bang startstate
  [to-draw si-render]
  [on-tick si-move]
  [stop-when si-game-over? si-render-final]
  [on-key si-control])
|#

;; sterowanie - strzałki
;; strzelanie - spacja



(check-expect(ufo-render (make-posn 50 60) BACKGROUND)
             (place-image UFO 50 60 BACKGROUND))

(check-expect(si-move(si-move startstate))
             (make-sigs (make-posn 100 12) (make-tank 12 1) #false))

         