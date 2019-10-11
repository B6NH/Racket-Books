;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Fire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 250)
(define WIDTH 400)
(define FOREST-HEIGHT 40)
(define FOREST(rectangle WIDTH FOREST-HEIGHT "solid" "green"))
(define BACKGROUND
  (place-image FOREST
               (/(image-width FOREST)2)
               (- HEIGHT(/(image-height FOREST)2))
               (empty-scene  WIDTH HEIGHT)))

(define FIRE-HEIGHT 80)

(define PLANE(rectangle 30 8 "solid" "blue"))
(define FIRE(rectangle 20 80 "solid" "red"))
(define DROP(circle 2 "solid" "blue"))

(define PLANE-FROM-TOP 15)

(define-struct world[plane drops fires waterloads timeleft])
(define-struct plane[direction position])

(define(render-drops drops img)
  (cond
    ((empty? drops)img)
    (else
     (place-image DROP
                  (posn-x(first drops))
                  (posn-y(first drops))
                  (render-drops(rest drops)img)))))

(define(render-fires fires img)
  (cond
    ((empty? fires)img)
    (else
     (place-image FIRE
                  (first fires)
                  (- HEIGHT(/(image-height FIRE)2))
                  (render-fires(rest fires)img)))))


(define(render ws)
  (render-fires
   (world-fires ws)
   (render-drops(world-drops ws)
                (place-image PLANE(plane-position(world-plane ws))PLANE-FROM-TOP BACKGROUND))))

;; ------------------------------------------------------------------


(define(move-drops drops)
  (cond
    ((empty? drops)'())
    (else
     (if(>(posn-y(first drops))HEIGHT)
        (move-drops(rest drops))
        (cons
         (make-posn(posn-x(first drops))
                   (+(posn-y(first drops))1))
         (move-drops(rest drops)))))))

(define(move-plane plane)
  (make-plane(plane-direction plane)
             (+(plane-position plane)
               (if(string=?(plane-direction plane)"right")1 -1))))

(define(collision? fire drops)
  (cond
    ((empty? drops)#f)
    (else(or(and(<(abs(- fire(posn-x(first drops))))(/(image-width FIRE)2))
                (>(posn-y(first drops))(- HEIGHT FIRE-HEIGHT)))
            (collision? fire(rest drops))))))
            
(define(update-fires fires drops)
  (cond
    ((empty? fires)'())
    (else(if(collision?(first fires)drops)
            (update-fires(rest fires)drops)
            (cons(first fires)(update-fires(rest fires)drops))))))

(define(update ws)
  (make-world
   (move-plane(world-plane ws))
   (move-drops(world-drops ws))
   (update-fires(world-fires ws)(world-drops ws))
   (world-waterloads ws)
   (-(world-timeleft ws)1)))

(define(stop ws)
  (or(and(empty?(world-drops ws))
         (zero?(world-waterloads ws)))
     (empty?(world-fires ws))
     (<=(world-timeleft ws)0)))

(define(drop-water ws)
  (if(>(world-waterloads ws)0)
     (make-world
      (world-plane ws)
      (cons(make-posn(plane-position(world-plane ws))
                     PLANE-FROM-TOP)
           (world-drops ws))
      (world-fires ws)
      (-(world-waterloads ws)1)
      (world-timeleft ws))
     ws))

(define(control ws ke)
  (cond
    ((or(string=? ke "right")
        (string=? ke "left"))
     (make-world
      (make-plane ke(plane-position(world-plane ws)))
      (world-drops ws)
      (world-fires ws)
      (world-waterloads ws)
      (world-timeleft ws)))
    ((string=? ke " ")
     (drop-water ws))
    (else ws)))

(define(lastscene ws)
  (overlay
   (text(if(empty?(world-fires ws))
           "WIN" "LOSE")
        30
        (if(empty?(world-fires ws))
           "green" "red"))
   (render ws)))
        

(define(create-fires ws num)
  (cond
    ((zero? num)ws)
    (else(create-fires
          (make-world
           (world-plane ws)
           (world-drops ws)
           (cons(random WIDTH)(world-fires ws))
           (world-waterloads ws)
           (world-timeleft ws))
          (- num 1)))))

;; ------------------------------------------------------------------

(define sample
  (make-world
   (make-plane "right" 40);; plane
   '();; drops
   '() ;; fires
   10;; waterloads
   1000));; timeleft
                    
(define(main ws numfires)
  (big-bang (create-fires ws numfires)
    (to-draw render)
    (on-tick update)
    (on-key control)
    (stop-when stop lastscene)
    (state #t)))

;; ------------------------------------------------------------------

(check-expect
 (control sample " ")
 (make-world
  (make-plane "right" 40)
  (list (make-posn 40 PLANE-FROM-TOP))
  '()
  9
  1000))

(check-expect
 (control sample "left")
 (make-world
  (make-plane "left" 40)
  '()
  '()
  10
  1000))


(check-expect
 (control sample "right")
 sample)

(check-expect
 (update sample)
 (make-world
  (make-plane "right" 41)
  '()
  '()
  10
  999))      


;;(main sample 7)

;; control - left and right arrow
;; use water - space
;; extinguish all fires with 10 waterloads in limited amount of time

