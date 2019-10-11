;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Zoo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define BACKGROUND(empty-scene 300 200))

(define CAT(circle 15 "solid" "brown"))
(define CHAM(circle 15 "solid" "green"))

(define-struct VANIMAL[x direction])

(define-struct VCHAM[x direction color])
(define-struct VCAT[x direction hapiness])

(define-struct ZOO[cham cat])

(define(render-cham-color color)
  (overlay CHAM(circle 20 "solid" color)))

(define examplezoo(make-ZOO(make-VCHAM 100 "right" "red")
                           (make-VCAT 100 "left" 170)))

(define(render zoo)
  (place-image(rectangle (VCAT-hapiness(ZOO-cat zoo)) 20 "solid" "red")140 170
              (place-image
               (render-cham-color (VCHAM-color(ZOO-cham zoo)))
               (VCHAM-x(ZOO-cham zoo)) 100
               (place-image CAT (VCAT-x(ZOO-cat zoo)) 50 BACKGROUND))))


(define(update zoo)
  (make-ZOO
   (make-VCHAM
    (+(VCHAM-x(ZOO-cham zoo))(if(string=?(VCHAM-direction (ZOO-cham zoo))"right")4 -4))


    (if(>=(VCHAM-x (ZOO-cham zoo))(image-width BACKGROUND))
               "left" 
               (if(<=(VCHAM-x (ZOO-cham zoo))0)
                  "right"
                  (VCHAM-direction (ZOO-cham zoo))))


    (VCHAM-color(ZOO-cham zoo)))
   (make-VCAT
    (+(VCAT-x(ZOO-cat zoo))(if(string=?(VCAT-direction (ZOO-cat zoo))"right")4 -4))


    (if(>=(VCAT-x (ZOO-cat zoo))(image-width BACKGROUND))
               "left" 
               (if(<=(VCAT-x (ZOO-cat zoo))0)
                  "right"
                  (VCAT-direction (ZOO-cat zoo))))
    (-(VCAT-hapiness(ZOO-cat zoo))2))))


(define(control zoo ke)
  (cond
    ((string=? ke "p")(make-ZOO(ZOO-cham zoo)
                               (make-VCAT
                                (VCAT-x(ZOO-cat zoo))
                                (VCAT-direction(ZOO-cat zoo))
                                (+(VCAT-hapiness(ZOO-cat zoo))30))))
    (else(make-ZOO
          (make-VCHAM
           (VCHAM-x(ZOO-cham zoo))
           (VCHAM-direction(ZOO-cham zoo))
           (cond
             ((string=? ke "r")"red")
             ((string=? ke "g")"green")
             ((string=? ke "b")"blue")
             (else(VCHAM-color(ZOO-cham zoo)))))
          (ZOO-cat zoo)))))

(define(stop zoo)
  (<=(VCAT-hapiness(ZOO-cat zoo))0))

;;(render examplezoo)

#|
(big-bang examplezoo
  (on-tick update)
  (to-draw render)
  (on-key control)
  (stop-when stop)
  (check-with ZOO?))
|#


(check-expect
 (update examplezoo)
 (make-ZOO (make-VCHAM 104 "right" "red") (make-VCAT 96 "left" 168)))

;; klawisze r g b, zmiana koloru kameleona
;; klawisz p, karmienie kota