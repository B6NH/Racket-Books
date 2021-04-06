;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname UfoTank2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define HEIGHT 300)
(define WIDTH 300)
(define BACKGROUND(overlay(rectangle WIDTH HEIGHT "solid" "cornsilk")(empty-scene WIDTH HEIGHT)))
(define UFO(circle 10 "solid" "green"))
(define TANK(rectangle 30 10 "solid" "brown"))
(define BULLET(circle 2 "solid" "purple"))
(define BULLET-SPEED 2)

(define-struct world[ufo tank bullets])
(define-struct ufo[x y])
(define-struct tank[position direction])
(define-struct bullet[x y])


(define(render-tank ws img)
  (place-image TANK
               (tank-position(world-tank ws))
               (-(image-height BACKGROUND)(/(image-height TANK)2))
               img))

(define(render-ufo ws img)
  (place-image UFO
               (ufo-x(world-ufo ws))
               (ufo-y(world-ufo ws))
               img))

(define(render-bullet bullet img)
  (place-image BULLET
               (bullet-x bullet)
               (bullet-y bullet)
               img))

(define(render-bullets bullets img)
  (cond
    ((empty? bullets)img)
    (else(render-bullet(first bullets)
                       (render-bullets(rest bullets)img)))))

  
(define(render ws)
  (render-bullets(world-bullets ws)(render-ufo ws(render-tank ws BACKGROUND))))


;; -------------------------------------------------------------------------
(define(move-ufo ufo)
  (make-ufo(if(zero?(modulo(ufo-y ufo) 80))
              (random(image-width BACKGROUND))
              (ufo-x ufo))
           (+(ufo-y ufo)1)))

(define(move-bullets bullets)
  (cond
    ((empty? bullets)'())
    ((< (bullet-y(first bullets)) 0)(move-bullets(rest bullets)))
    (else(cons(make-bullet(bullet-x(first bullets))
                          (-(bullet-y(first bullets))BULLET-SPEED))
              (move-bullets(rest bullets))))))

(define(move-tank tank)
  (make-tank
   (+(tank-position tank)
     (if(string=?(tank-direction tank)"right")
        4 -4))
   (tank-direction tank)))

(define(update ws)
  (make-world
   (move-ufo(world-ufo ws))
   (move-tank(world-tank ws))
   (move-bullets(world-bullets ws))))

(define(control ws ke)
  (cond
    ((or(string=? ke "left")(string=? ke "right"))
     (make-world(world-ufo ws)
                (make-tank(tank-position(world-tank ws))ke)
                (world-bullets ws)))
    ((string=? ke " ")
     (make-world(world-ufo ws)
                (world-tank ws)
                (cons(make-bullet
                      (tank-position(world-tank ws))
                      (-(image-height BACKGROUND)(image-height TANK)))
                     (world-bullets ws))))
     (else ws)))


(define(detect-collision bullet ufo)
  (and(> (/(image-width UFO)2)(abs(-(ufo-x ufo)(bullet-x bullet))))
      (> (/(image-width UFO)2)(abs(-(ufo-y ufo)(bullet-y bullet))))))

(define(detect-collisions ws)
  (cond
    ((empty?(world-bullets ws))#f)
    (else
     (or(detect-collision(first(world-bullets ws))(world-ufo ws))
        (detect-collisions(make-world(world-ufo ws)
                                     (world-tank ws)
                                     (rest(world-bullets ws))))))))


(define(contact? ws)
  (>(ufo-y(world-ufo ws))(- HEIGHT(/(image-height UFO)2))))


(define(stop ws)
  (or(contact? ws)
     (detect-collisions ws)))

(define(lastscene ws)
  (overlay
   (if(>(ufo-y(world-ufo ws))(- HEIGHT(/(image-height UFO)2)))
      (text "lose" 25 "red")
      (text "win" 25 "green"))(render ws)))
;; --------------------------------------------------------------------
(define sample
  (make-world(make-ufo 100 20)
             (make-tank 20 "right")
             '()))

;; --------------------------------------------------------------------
(define(main ws)
  (big-bang ws
    (to-draw render)
    (on-tick update)
    (on-key control)
    (stop-when stop lastscene)))
;; --------------------------------------------------------------------

(check-expect(control sample "right")sample)
(check-expect(control sample "left")
             (make-world(make-ufo 100 20)
                        (make-tank 20 "left")
                        '()))
(check-expect(control sample " ")
             (make-world(make-ufo 100 20)
                        (make-tank 20 "right")
                        (list (make-bullet 20 290))))
;; --------------------------------------------------------------------

;;(main sample)