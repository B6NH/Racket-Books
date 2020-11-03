;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

 
(define-struct transition [current next])

(define-struct fs [fsm current]);; fsm is list of transitions, current is state

(define(state=? one two)
  (string=? one two))


;; finite state machines
(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define bw-machine
  (list(make-transition "black" "white")
       (make-transition "white" "black")))
;; ---------------------------------------------------

(define(find fsm current)
  (cond
    ((empty? fsm)(error (string-append "not found: " current)))
    ((state=?(transition-current(first fsm))current)
     (transition-next(first fsm)))
    (else(find(rest fsm)current))))


(define (render an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm))))

;; VERSION 1(uses state - machine and current string)
(define (simulate.v1 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw render]
    [on-key find-next-state]))


(check-expect (render
                (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black")
             "not found: black")
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))

(check-expect
  (find-next-state (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))

;;(simulate.v1 fsm-traffic "red")
;;(simulate.v1 bw-machine "black")

;; --------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------

(define-struct ktransition [current key next])

;; start middle end
(define oldmachine
  (list(make-ktransition "start" "a" "middle")
       (make-ktransition "middle" "b" "middle")
       (make-ktransition "middle" "c" "middle")
       (make-ktransition "middle" "d" "end")))

(define(render2 fs)
  (square 100 "solid"
          (cond
            ((string=?(fs-current fs)"start")"white")
            ((string=?(fs-current fs)"middle")"green")
            ((string=?(fs-current fs)"end")"blue"))))

;; find next state (string)
(define(find2 fsm current ke)
  (cond
    ((empty? fsm)(error (string-append "not found: " current)))
    ((and(state=?(ktransition-current(first fsm))current)
         (key=?(ktransition-key(first fsm))ke))
     (ktransition-next(first fsm)))
    (else(find2(rest fsm)current ke))))
  
;; find next fs
(define(find-next-state2 an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find2 (fs-fsm an-fsm) (fs-current an-fsm)ke)))


;; VERSION 2(uses state - machine with keys and current string)
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw render2]
    [on-key find-next-state2]))

;;(simulate.v2 oldmachine "start")


;; --------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------------


(define-struct fsm3 [initial transitions final])

(define(render3 fsm)
  (square 100 "solid"(fsm3-initial fsm)))

(define(lastscene fsm)(square 100 "solid"(fsm3-final fsm)))
  
(define modernmachine
  (make-fsm3 "white"
             (list(make-ktransition "white" "a" "green")
                  (make-ktransition "green" "b" "green")
                  (make-ktransition "green" "c" "green")
                  (make-ktransition "green" "d" "blue"))
             "blue"))
  

(define(find-next-state3 fsm ke)
  (make-fsm3
   (find2(fsm3-transitions fsm);; find next color
         (fsm3-initial fsm)
         ke)
   (fsm3-transitions fsm)
   (fsm3-final fsm)))

(define(stop fsm)
  (string=?(fsm3-initial fsm)
           (fsm3-final fsm)))


;; VERSION 3(uses only machine - everything is inside)
(define (fsm-simulate fsm)
  (big-bang fsm
    [to-draw render3]
    [on-key find-next-state3]
    [stop-when stop lastscene]
    [state #f]))

;;(fsm-simulate modernmachine)