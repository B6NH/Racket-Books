;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 20)
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))
(define BACKGROUND(empty-scene SCENE-SIZE(* SIZE HEIGHT)))
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape]);; block is falling block, landscape is list of resting blocks
(define-struct block [x y])
 

(define landscape0 '())
(define block-dropping(make-block 2 5))
(define tetris0(make-tetris block-dropping landscape0))

(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))

(define tetris1(make-tetris block-dropping(list block-landed)))
(define tetris2(make-tetris block-dropping(list  block-on-block block-landed)))

(define(render-one-block block img)
  (place-image BLOCK
               (+(*(block-x block)WIDTH)(/ WIDTH 2))
               (+(*(block-y block)WIDTH)(/ WIDTH 2))
               img))

(define(render-all-blocks blocks img)
  (cond
    ((empty? blocks)img)
    (else(render-one-block(first blocks)
                          (render-all-blocks(rest blocks)img)))))

(define(render ws)
  (render-one-block(tetris-block ws)
                   (render-all-blocks(tetris-landscape ws)
                   BACKGROUND)))

;; -----------------------------------------------------------------------
(check-expect(render tetris0)(place-image BLOCK 25 55 BACKGROUND))
(check-expect
 (render tetris1)
 (place-image BLOCK 25 55
              (place-image BLOCK 5 (-(image-height BACKGROUND)5)
                           BACKGROUND)))
(check-expect
 (render tetris2)
 (place-image BLOCK 25 55
              (place-image BLOCK 5 (-(-(image-height BACKGROUND)5)SIZE);; upper block
                           (place-image BLOCK 5 (-(image-height BACKGROUND)5) BACKGROUND))));; lower block

;; most left position
(check-expect(control(make-tetris(make-block 0 2)'())"left")(make-tetris(make-block 0 2)'()))
;; most right position
(check-expect(control(make-tetris(make-block 9 2)'())"right")(make-tetris(make-block 9 2)'()))

;; -----------------------------------------------------------------------
(define(is-block-on-blocks? ws)
  (member?(make-block(block-x(tetris-block ws))
                     (+(block-y(tetris-block ws))1))
          (tetris-landscape ws)))

(define(is-block-on-bottom? ws)
  (>=(+(block-y(tetris-block ws))1)HEIGHT))

(define(create-block-on-right ws)
  (make-block
   (if(>=(block-x(tetris-block ws))(- WIDTH 1));; if is on most righ column
      0 ;; create it in most left column
      (+(block-x(tetris-block ws))1))
   0))

(define(add-block-to-landscape-and-create-new ws)
  (make-tetris(create-block-on-right ws)
              (cons(tetris-block ws)(tetris-landscape ws))))

(define(move-block-down ws)
  (make-tetris
   (make-block(block-x(tetris-block ws))
              (+(block-y(tetris-block ws))1))
   (tetris-landscape ws)))
  
(define(update ws)
  (if(or(is-block-on-blocks? ws)
        (is-block-on-bottom? ws))
     (add-block-to-landscape-and-create-new ws)
     (move-block-down ws)))

;; -----------------------------------------------------------------------

(define(can-move-left? ws)
  (and(>=(-(block-x(tetris-block ws))1)0)
      (not(member?(make-block(-(block-x(tetris-block ws))1)
                             (block-y(tetris-block ws)))
                  (tetris-landscape ws)))))

(define(can-move-right? ws)
  (and(<(+(block-x(tetris-block ws))1)WIDTH)
      (not(member?(make-block(+(block-x(tetris-block ws))1)
                             (block-y(tetris-block ws)))   
                  (tetris-landscape ws)))))

(define(move-right ws)
  (make-tetris
   (make-block
    (if(can-move-right? ws)
       (+(block-x(tetris-block ws))1)
       (block-x(tetris-block ws)))
    (block-y(tetris-block ws)))
   (tetris-landscape ws)))


(define(move-left ws)
  (make-tetris
   (make-block
    (if(can-move-left? ws)
       (-(block-x(tetris-block ws))1)
       (block-x(tetris-block ws)))
    (block-y(tetris-block ws)))
   (tetris-landscape ws)))
;; -----------------------------------------------------------------------
(define(control ws ke)
  (cond
    ((string=? ke "left")
     (move-left ws))
    ((string=? ke "right")
     (move-right ws))
    (else ws)))


;; check every block
#|
(define(column-too-high landscape)
  (cond
    ((empty? landscape)#f)
    (else(or(<=(block-y(first landscape))0)
            (column-too-high(rest landscape))))))
|#


;; check only first block(last added)
(define(column-too-high landscape)
  (if(empty? landscape)
     #false
     (<=(block-y(first landscape))0)))

(define(stop ws)
  (column-too-high(tetris-landscape ws)))


(define(last-scene ws)
  (overlay(text(string-append
                (number->string(length(tetris-landscape ws)))
                " blocks")
               15 "red")BACKGROUND))

(define sample(make-tetris(make-block 1 0)
                          '()))

(define(main rate)
  (big-bang sample
    (to-draw render)
    (on-tick update rate)
    (on-key control)
    (stop-when stop last-scene)))

;;(main 0.2)


  