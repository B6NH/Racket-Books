;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 300)
(define WIDTH 300)
(define BACKGROUND
  (overlay(rectangle WIDTH HEIGHT "solid" "blue")
          (empty-scene WIDTH HEIGHT)))
(define SEGMENT(circle 5 "solid" "green"))
(define BORDERTEXT "worm hit border")
(define SUICIDETEXT "worm ate itself")
(define TEXTSIZE 16)
(define TEXTCOLOR "red")
(define FOOD(circle 5 "solid" "orange"))

(define-struct snake[direction tail food])

(define(render-segment pos img)
  (place-image
   SEGMENT
   (+(*(posn-x pos)(image-width SEGMENT))
     (/(image-width SEGMENT)2))
   (+(*(posn-y pos)(image-width SEGMENT))
     (/(image-width SEGMENT)2))
   img))

(define(render-tail tail img)
  (cond
    ((empty? tail)img)
    (else
     (render-segment
      (first tail)
      (render-tail(rest tail)img)))))

(define(render-food ws img)
  (place-image FOOD
               (+(*(posn-x (snake-food ws))(image-width FOOD))
                 (/(image-width FOOD)2))
               (+(*(posn-y (snake-food ws))(image-width FOOD))
                 (/(image-width FOOD)2))img))


(define(render ws)
  (render-tail(snake-tail ws)(render-food ws BACKGROUND)))

(define(remove-last lst)
  (cond
    ((empty?(rest lst))'())
    (else(cons(car lst)(remove-last(rest lst))))))

(define(eating? ws)
  (equal?(first(snake-tail ws))(snake-food ws)))


(define(update ws)
  (make-snake
   (snake-direction ws)
   (cons
    (cond
      ((string=?(snake-direction ws)"right")
       (make-posn(+(posn-x(first(snake-tail ws)))
                   1)
                 (posn-y(first(snake-tail ws)))))
      ((string=?(snake-direction ws)"left")
       (make-posn(-(posn-x(first(snake-tail ws)))
                   1)
                 (posn-y(first(snake-tail ws)))))
      ((string=?(snake-direction ws)"up")
       (make-posn(posn-x(first(snake-tail ws)))
                 (-(posn-y(first(snake-tail ws)))
                   1)))
      ((string=?(snake-direction ws)"down")
       (make-posn(posn-x(first(snake-tail ws)))
                 (+(posn-y(first(snake-tail ws)))
                   1)))
      (else(error "wrong direction")))
    (if(eating? ws)
       (snake-tail ws)
       (remove-last(snake-tail ws))))
   (if(eating? ws)
      (food-create(snake-food ws))
      (snake-food ws))))

(define(food-create old-posn)
  (local((define new-posn(make-posn(random (/ WIDTH 10))(random(/ WIDTH 10)))))
    (if(and(=(posn-x old-posn)(posn-x new-posn))
           (=(posn-y old-posn)(posn-y new-posn)))
       (food-create old-posn)
       new-posn)))

(define(control ws ke)
  (make-snake
   (if(or(string=? ke "up")
         (string=? ke "down")
         (string=? ke "left")
         (string=? ke "right"))
      ke
      (snake-direction ws))
   (snake-tail ws)
   (snake-food ws)))


(define(stop ws)
  (or(<(posn-x(first(snake-tail ws)))0)
     (<(posn-y(first(snake-tail ws)))0)
     (>=(posn-x(first(snake-tail ws)))(/ WIDTH 10))
     (>=(posn-y(first(snake-tail ws)))(/ WIDTH 10))
     (member?(first(snake-tail ws))
             (rest(snake-tail ws)))))


(define(finalscene ws)
  (place-image
   (text(if(member?(first(snake-tail ws))
                   (rest(snake-tail ws)))
           SUICIDETEXT BORDERTEXT) TEXTSIZE TEXTCOLOR)
   (/(image-width(text(if(member?(first(snake-tail ws))
                                 (rest(snake-tail ws)))
                         SUICIDETEXT BORDERTEXT) TEXTSIZE TEXTCOLOR))2)
   (- HEIGHT (image-width SEGMENT))
   (render ws)))


(define(main ws showstate)
  (length
   (snake-tail
    (big-bang ws
      (state showstate)
      (on-tick update 0.3)
      (to-draw render)
      (on-key control)
      (stop-when stop finalscene)))))

(define sample
  (make-snake
   "right"
   (list(make-posn 4 1)
        (make-posn 3 1)
        (make-posn 2 1))
   (make-posn 6 2)))


(check-expect(update sample)
             (make-snake "right"
                         (list(make-posn 5 1)
                              (make-posn 4 1)
                              (make-posn 3 1))
                         (make-posn 6 2)))


;;(main sample #f)

;; control with arrows
