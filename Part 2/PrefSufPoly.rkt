;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname PrefSufPoly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define(suffixes lst)
  (cond
    ((empty? lst)'())
    (else(cons lst(suffixes(rest lst))))))

(define(prefixes lst)
  (cond
    ((empty? lst)'())
    (else(cons lst(prefixes(reverse(rest(reverse lst))))))))

;;(suffixes(list "a" "b" "c" "d"))
;;(prefixes(list "a" "b" "c" "d"))

; -----------------------------------------------------------------------

(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
		
(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

; a plain background image 
(define MT (empty-scene 50 50))
 

;; bad solution
;; Image Poly -> Image
(define (render-poly img p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
       (render-line
         (render-line MT (first p) (second p))
         (second p) (third p))
       (third p) (first p))]
    [else
     (render-line (render-poly img (rest p))
                  (first p)
                  (second p))]))



(define(add-at-end item lst)
  (cond
    ((empty? lst)(cons item '()))
    (else(cons(first lst)(add-at-end item(rest lst))))))


(check-expect
 (add-at-end(make-posn 77 66)(list(make-posn 12 66)(make-posn 45 11)))
 (list(make-posn 12 66)(make-posn 45 11)(make-posn 77 66)))


;; ORIGINAL

(define (render-polygon img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))


#| alternative solutions


(define (render-polygon img p)
  (connect-dots img(add-at-end(first p)p)))


(define(render-polygon img p)
  (connect-dots img(cons(last p)p)))

(define (render-polygon img p)
  (connect-dots2 img p(first p)))

|#

(check-expect
  (render-polygon MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))


(check-expect
  (render-polygon MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))



; Image Posn Posn -> Image 
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
    img
    (posn-x p) (posn-y p) (posn-x q) (posn-y q)
    "red"))


(check-expect(render-line MT(make-posn 20 20)(make-posn 30 30))
             (scene+line MT 20 20 30 30 "red"))


; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
(define (connect-dots img p)
  (cond
    ((empty?(rest p))img)
    (else
     (render-line
      (connect-dots img(rest p))
      (first p)(second p)))))


(define(connect-dots2 img p lastelem)
  (render-line(connect-dots img p)
              (last p)lastelem))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))


(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 10 20 20 "red")
               20 20 30 20 "red"))

(check-expect (connect-dots2 MT triangle-p(make-posn 40 40))
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 40 40 "red"))
