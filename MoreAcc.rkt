;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname MoreAcc) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; 33 More Uses of Accumulation

;; Exercise 511. Explain the scope of each binding occurrence

; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))
(define ex5 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))
(define ex6 '((λ (x) x) (λ (x) x)))


;; Exercise 512. Define is-var?, is-λ?, and is-app?
(check-expect(is-var? ex1)#false)
(check-expect(is-var? 'something)#true)
(check-expect(is-λ? ex1)#true)
(check-expect(is-λ? ex2)#true)
(check-expect(is-λ? ex3)#true)
(check-expect(is-λ? ex4)#false)
(check-expect(is-λ? ex5)#false)
(check-expect(is-app? ex1)#false)
(check-expect(is-app? ex2)#false)
(check-expect(is-app? ex3)#false)
(check-expect(is-app? ex4)#true)
(check-expect(is-app? ex5)#true)
(check-expect(is-app? 'z)#false)
;; -----------------------------------------------------------------
(define(is-var? x)(symbol? x))

(define(is-λ? x)(and(cons? x)(=(length x)3)))

(define(is-app? x)(and(cons? x)(=(length x)2)))

(define(λ-para lmbd)(first(second lmbd)))

(define(λ-body lmbd)(third lmbd))

(define(app-fun app)(first app))

(define(app-arg app)(second app))
;; -----------------------------------------------------------------
(check-expect
 (λ-para(list 'λ (list 'name) 'sss))
 'name)
(check-expect
 (λ-body(list 'λ (list 'name) 'sss))
 'sss)
(check-expect
 (app-fun'((λ (x) (x x)) (λ (x) (x x))))
 '(λ (x) (x x)))
(check-expect
 (app-arg'((λ (x) (x x)) (λ (x) (x x))))
 '(λ (x) (x x)))
(check-expect
 (app-arg'((λ (x) x)g))
 'g)


(check-expect (undeclareds ex1) '(λ (x)*declared:x))
(check-expect (undeclareds ex2) '(λ (x)*undeclared:y))

(check-expect (undeclareds ex3) '(λ (y) (λ (x)*declared:y)))
(check-expect (undeclareds ex4) '((λ (x) (*declared:x *declared:x))
                                  (λ (x) (*declared:x *declared:x))))

(check-expect (undeclareds ex5) '(((λ (y) (λ (x) *declared:y)) (λ (z) *declared:z)) (λ (w) *declared:w)))
(check-expect (undeclareds ex6) '((λ (x) *declared:x) (λ (x)*declared:x)))


; Lam -> Lam 
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (string->symbol(string-append(symbol->string(if (member? le declareds)'*declared '*undeclared))
                                            ":"
                                            (symbol->string le)))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                   (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
               (list (undeclareds/a fun declareds)
                     (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))


;; Design declareds, which produces the list of all symbols

(define(declareds le)
  (cond
    ((is-var? le)'())
    ((is-λ? le)(append(list(λ-para le))(declareds(λ-body le))))
    ((is-app? le)(append(declareds(app-fun le))
                        (declareds(app-arg le))))))


(check-expect(declareds ex1)(list 'x))
(check-expect(declareds ex2)(list 'x))
(check-expect(declareds ex3)(list 'y 'x))
(check-expect(declareds ex4)(list 'x 'x))
(check-expect(declareds ex5)(list 'y 'x 'z 'w))
(check-expect(declareds ex6)(list 'x 'x))

;; Exercise 513. Develop a data representation for the same

(define-struct sym [ symbol ])
(define-struct lam [ param body ])
(define-struct appl [ fun arg ])

(define ex11(make-lam(make-sym 'x)(make-sym 'x)))
(define ex22(make-lam(make-sym 'x)(make-sym 'y)))
(define ex33(make-lam(make-sym 'y)
                     (make-lam(make-sym 'x)
                              (make-sym 'y))))
(define ex44(make-appl
             (make-lam(make-sym 'x)(make-appl(make-sym 'x)(make-sym 'x)))
             (make-lam(make-sym 'x)(make-appl(make-sym 'x)(make-sym 'x)))))
(define ex55(make-appl
             (make-appl (make-lam(make-sym 'y)(make-lam(make-sym 'x)(make-sym 'y)))
                        (make-lam(make-sym 'z)(make-sym 'z)))
             (make-lam(make-sym 'w)(make-sym 'w))))


;; Exercise 514. Make up an ISL+ expression in which x

(check-expect
 (undeclareds'((λ (x) x) (λ (y) x)))
 (list (list 'λ (list 'x)'*declared:x) (list 'λ (list 'y)'*undeclared:x)))


(check-expect
 (undeclareds'((λ (x) y) (λ (x) x)))
 (list (list 'λ (list 'x)'*undeclared:y) (list 'λ (list 'x)'*declared:x)))


;; Exercise 516. Redesign the undeclareds function

; Lam -> Lam 
(define (undeclareds.v2 le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(sym? le)
               (local((define symb(sym-symbol le)))
                 (make-sym(string->symbol(string-append(symbol->string(if (member? le declareds)'*declared '*undeclared))
                                                       ":"
                                                       (symbol->string symb)))))]
              [(lam? le)
               (local ((define para (lam-param le))
                       (define body (lam-body le))
                       (define newd (cons para declareds)))
                 (make-lam para
                   (undeclareds/a body newd)))]

              [(appl? le)
               (local ((define fun (appl-fun le))
                       (define arg (appl-arg le)))
                 (make-appl (undeclareds/a fun declareds)
                            (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))


(check-expect(undeclareds.v2 ex11)(make-lam (make-sym 'x) (make-sym '*declared:x)))
(check-expect(undeclareds.v2 ex22)(make-lam (make-sym 'x) (make-sym '*undeclared:y)))
(check-expect(undeclareds.v2 ex33)(make-lam (make-sym 'y) (make-lam (make-sym 'x) (make-sym '*declared:y))))
(check-expect
 (undeclareds.v2(make-appl(make-lam(make-sym 'x)(make-sym 'x))
                          (make-lam(make-sym 'y)(make-sym 'x))))
 (make-appl (make-lam (make-sym 'x) (make-sym '*declared:x))
            (make-lam (make-sym 'y) (make-sym '*undeclared:x))))

(check-expect
 (undeclareds.v2 ex44)
 (make-appl
  (make-lam(make-sym 'x)(make-appl(make-sym '*declared:x)(make-sym '*declared:x)))
  (make-lam(make-sym 'x)(make-appl(make-sym '*declared:x)(make-sym '*declared:x)))))

(check-expect
 (undeclareds.v2 ex55)
 (make-appl
  (make-appl (make-lam (make-sym 'y) (make-lam (make-sym 'x) (make-sym '*declared:y)))
             (make-lam (make-sym 'z) (make-sym '*declared:z)))
  (make-lam (make-sym 'w) (make-sym '*declared:w))))


;; Exercise 517. Design static-distance.

; Lam -> Lam 
(define (static-distance le0)
  (local ((define(find-distance le declareds)
            (local((define(h dec count)
                     (cond
                       ((empty? dec)#false)
                       (else(if(equal? le(first dec))
                               count
                               (h(rest dec)(add1 count)))))))
              (h declareds 0)))
          (define (undeclareds/a le declareds)
            (cond
              [(sym? le)
               (local((define symb(sym-symbol le))
                      (define found-index(find-distance le declareds)))
                 (if(not(boolean? found-index))
                    found-index
                    (make-sym(string-append(symbol->string '*undeclared)":"(symbol->string symb)))))]
              [(lam? le)
               (local ((define para (lam-param le))
                       (define body (lam-body le))
                       (define newd (cons para declareds)))
                 (make-lam para
                   (undeclareds/a body newd)))]

              [(appl? le)
               (local ((define fun (appl-fun le))
                       (define arg (appl-arg le)))
                 (make-appl (undeclareds/a fun declareds)
                            (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))



(check-expect(static-distance ex11)(make-lam (make-sym 'x) 0))

;'((λ (x) ((λ (y) (y x)) x)) (λ (z) z))
(define ex-stat
  (make-appl
   (make-lam(make-sym 'x)
            (make-appl
             (make-lam(make-sym 'y)(make-appl(make-sym 'y)(make-sym 'x)))
             (make-sym 'x)))
   (make-lam(make-sym 'z)(make-sym 'z))))

(check-expect
 (static-distance ex-stat)
 (make-appl
  (make-lam (make-sym 'x)
            (make-appl
             (make-lam (make-sym 'y) (make-appl 0 1))
             0))
  (make-lam (make-sym 'z) 0)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------


;; 33.2 Data Representations with Accumulators


;; Sample Problem Your manager tells you the following story.


; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination ???

(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (local((define final(first (filter final? los))))
                 (cons(make-world(world-left final)
                                 (world-right final)
                                 (world-boat final)
                                 '())
                      (world-states final)))]
              
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))


;; Exercise 521. Develop a representation for the states

(define-struct world [ left right boat states])
(define-struct side [ missionars cannibals ])

(define initial-puzzle(make-world(make-side 3 3)(make-side 0 0)'l'()))
(define middle1(make-world(make-side 3 2)(make-side 0 1)'r'()))
(define middle2(make-world(make-side 3 1)(make-side 0 2)'r'()))
(define middle3(make-world(make-side 2 2)(make-side 1 1)'r'()))
(define final-puzzle(make-world(make-side 0 0) (make-side 3 3)'r'()))

(define(final? a-world)
  (local((define left(world-left a-world))
         (define right(world-right a-world)))
    (and(zero?(side-missionars left))
        (zero?(side-cannibals left))
        (= 3(side-missionars right))
        (= 3(side-missionars right))
        (symbol=?(world-boat a-world)'r))))

(check-satisfied final-puzzle final?)
(check-expect(final? initial-puzzle)#false)
(check-expect(final? middle1)#false)
(check-expect(final? middle2)#false)
(check-expect(final? middle3)#false)

(define MISSIONARY(circle 10 "solid" "black"))
(define CANNIBAL(circle 10 "solid" "red"))
(define BOAT(rectangle 40 20 "solid" "green"))
(define BACKGROUND(empty-scene 300 300))
(define LEFT-PEOPLE-ROW-X 50)
(define RIGHT-PEOPLE-ROW-X 250)
(define MISSIONARY-INITIAL-Y 50)
(define CANNIBAL-INITIAL-Y 140)
(define DISTANCE-BETWEEN-PEOPLE 25)
(define BOAT-X-LEFT 100)
(define BOAT-X-RIGHT 200)
(define BOAT-Y 150)
(define RIVER-WIDTH 100)

(define(render-river img)
  (place-image(rectangle RIVER-WIDTH 300 "solid" "blue")150 150 img))

(define(render-boat a-world img)
  (place-image BOAT(cond
                     ((symbol=?(world-boat a-world)'l)BOAT-X-LEFT)
                     ((symbol=?(world-boat a-world)'r)BOAT-X-RIGHT))BOAT-Y img))


;; renders num of who images on image, at x, starting with height y
(define(render-people who num img x y)
  (local((define(h nu y)
           (cond
             ((zero? nu)img)
             (else(place-image who x y
                               (h(sub1 nu)(+ y DISTANCE-BETWEEN-PEOPLE)))))))
    (h num y)))


(define(render-side a-side image row-x)
  (render-people CANNIBAL
                 (side-cannibals a-side);; left cannibals
                 (render-people MISSIONARY
                                (side-missionars a-side);; left missionars
                                image
                                row-x MISSIONARY-INITIAL-Y)
                 row-x CANNIBAL-INITIAL-Y))
  

(define(render-left a-world image)
  (render-side(world-left a-world)image LEFT-PEOPLE-ROW-X))

(define(render-right a-world image)
  (render-side(world-right a-world)image RIGHT-PEOPLE-ROW-X))

;(render-people MISSIONARY 3(empty-scene 300 300)50 50)


(define(render-mc a-world)
  (render-right a-world
                (render-left a-world
                             (render-boat a-world(render-river BACKGROUND)))))

#|
(render-mc initial-puzzle)
(render-mc middle1)
(render-mc middle2)
(render-mc middle3)
(render-mc final-puzzle)
|#

;; Exercise 523. Design the create-next-states function.

(check-expect
 (move-right initial-puzzle 1 1)
 (list(make-world (make-side 2 2) (make-side 1 1) 'r '())))


;; too few missionars on left
(check-expect
 (move-right initial-puzzle 2 0)
 '())
(check-expect
 (move-right initial-puzzle 1 0)
 '())
(check-error
 (move-right initial-puzzle 0 3)
 "too many people on boat")

(check-expect
 (move-left(make-world (make-side 0 0) (make-side 3 3) 'r '())1 1)
 (list(make-world (make-side 1 1) (make-side 2 2) 'l '())))

(check-satisfied(make-side 3 2)is-side-valid?)
(check-satisfied(make-side 3 1)is-side-valid?)
(check-satisfied(make-side 0 1)is-side-valid?)
(check-satisfied(make-side 0 2)is-side-valid?)


;; bug here
(define(is-side-valid? side)
  (local((define missionars(side-missionars side))
         (define cannibals(side-cannibals side)))
    (and(>= 3 missionars 0)
        (>= 3 cannibals 0)
        (if(> missionars 0)(>= missionars cannibals)#true))))

(define(generic-move world miss-num can-num a b new-boat)
  (if(>(+ miss-num can-num)2)
     (error "too many people on boat")
     (local((define sidel(world-left world))
            (define sider(world-right world))
            (define answer
              (make-world(make-side(a(side-missionars sidel)miss-num)
                                   (a(side-cannibals sidel)can-num))
                         (make-side(b(side-missionars sider)miss-num)
                                   (b(side-cannibals sider)can-num))
                         new-boat
                         '())))
       (if(and(is-side-valid?(world-left answer))
              (is-side-valid?(world-right answer)))
          (list answer)
          '()))))

(define(move-right world miss-num can-num)
  (generic-move world miss-num can-num - + 'r))


(define(move-left world miss-num can-num)
  (generic-move world miss-num can-num + - 'l))


(define list-of-states(list initial-puzzle))

(define(generic-create-states a-world mf)
  (append
   (mf a-world 1 1)
   (mf a-world 1 0)
   (mf a-world 0 1)
   (mf a-world 2 0)
   (mf a-world 0 2)))

(define(create-states-from-left a-world)
  (generic-create-states a-world move-right))

(define(create-states-from-right a-world)
  (generic-create-states a-world move-left))
  


(define(create-states-for-one a-world)
  (local((define boat(world-boat a-world))
         (define new-states
           (cond
             ((symbol=? boat 'l)(create-states-from-left a-world))
             ((symbol=? boat 'r)(create-states-from-right a-world))))
         (define seen-states(world-states a-world))
         (define unseen-states(filter(lambda(z)(not(member? z seen-states)))new-states)))

    ;; add current state to all next states
    (map(lambda(x)(make-world(world-left x)
                             (world-right x)
                             (world-boat x)
                             (cons(make-world(world-left a-world)(world-right a-world)(world-boat a-world)'())seen-states)))
        unseen-states)))

(define(create-next-states states)
  (cond
    ((empty? states)'())
    (else(append(create-states-for-one(first states))
                (create-next-states(rest states))))))

(check-expect
 (create-states-from-left initial-puzzle)
 (list
  (make-world (make-side 2 2) (make-side 1 1) 'r '())
  (make-world (make-side 3 2) (make-side 0 1) 'r '())
  (make-world (make-side 3 1) (make-side 0 2) 'r '())))

(check-expect
 (create-states-for-one initial-puzzle)
 (list
  (make-world (make-side 2 2) (make-side 1 1) 'r (list initial-puzzle))
  (make-world (make-side 3 2) (make-side 0 1) 'r (list initial-puzzle))
  (make-world (make-side 3 1) (make-side 0 2) 'r (list initial-puzzle))))


(define solution(solve initial-puzzle))

(check-expect
 solution
 (list
  (make-world (make-side 0 0) (make-side 3 3) 'r '())
  (make-world (make-side 1 1) (make-side 2 2) 'l '())
  (make-world (make-side 0 1) (make-side 3 2) 'r '())
  (make-world (make-side 0 3) (make-side 3 0) 'l '())
  (make-world (make-side 0 2) (make-side 3 1) 'r '())
  (make-world (make-side 2 2) (make-side 1 1) 'l '())
  (make-world (make-side 1 1) (make-side 2 2) 'r '())
  (make-world (make-side 3 1) (make-side 0 2) 'l '())
  (make-world (make-side 3 0) (make-side 0 3) 'r '())
  (make-world (make-side 3 2) (make-side 0 1) 'l '())
  (make-world (make-side 2 2) (make-side 1 1) 'r '())
  (make-world (make-side 3 3) (make-side 0 0) 'l '())))


(define images(map(lambda(x)(render-mc x))solution))

;(create-states-for-one initial-puzzle)

;;(run-movie 4 (reverse images))