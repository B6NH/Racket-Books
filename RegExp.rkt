;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname RegExp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; state is start, middle, end, error

(define startstate "start")

(define(render state)
  (rectangle 100 100 "solid"
             (cond
               ((string=? state "start")"white")
               ((string=? state "middle")"yellow")
               ((string=? state "end")"green")
               (else "red"))))

(define(control state ke)
  (cond
    ((string=? state "start")(if(string=? ke "a")"middle" state))
    ((string=? state "middle")(cond
                                ((or(string=? ke "b")
                                    (string=? ke "c"))
                                 state)
                                ((string=? ke "d")"end")
                                (else "error")))
    (else state)))


#|
(big-bang startstate
  (to-draw render)
  (on-key control))
|#

(check-expect(render "middle")(rectangle 100 100 "solid" "yellow"))
(check-expect(render "start")(rectangle 100 100 "solid" "white"))
(check-expect(render "end")(rectangle 100 100 "solid" "green"))
(check-expect(render "error")(rectangle 100 100 "solid" "red"))



;; recognize regular expresion "a" -> arbitrary number of "b" and "c" -> end with "d"