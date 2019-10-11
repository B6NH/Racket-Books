;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Chess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; 29.2 Project: Backtracking

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column


;; Exercise 479. Design the threatening? function

(check-expect(threatening?(make-posn 0 0)(make-posn 2 1))#false)
(check-expect(threatening?(make-posn 2 0)(make-posn 0 1))#false)
(check-expect(threatening?(make-posn 0 2)(make-posn 2 1))#false)
(check-expect(threatening?(make-posn 3 0)(make-posn 1 3))#false)
(check-expect(threatening?(make-posn 0 2)(make-posn 3 1))#false)

(check-expect(threatening?(make-posn 0 0)(make-posn 0 2))#true);; hori
(check-expect(threatening?(make-posn 1 1)(make-posn 1 3))#true);; vert
(check-expect(threatening?(make-posn 0 0)(make-posn 2 2))#true);; diag-left
(check-expect(threatening?(make-posn 0 1)(make-posn 2 3))#true);; diag-left
(check-expect(threatening?(make-posn 3 0)(make-posn 1 2))#true);; diag-right
(check-expect(threatening?(make-posn 3 1)(make-posn 1 3))#true);; diag-right

(define(threatening? one two)
  (and(or(=(posn-x one)(posn-x two));; horizontal
         (=(posn-y one)(posn-y two));; vertical
         (=(abs(-(posn-x one)(posn-x two)));; diagonal left
           (abs(-(posn-y one)(posn-y two))))
         (=(+(posn-x one)(posn-y one));; diagonal right
           (+(posn-x two)(posn-y two))))
      (not(equal? one two))))
      
      
;; Exercise 480. Design render-queens

(define CELL-SIZE 50)
(define HALF-CELL(/ CELL-SIZE 2))
(define QUEEN-SIZE 15)
(define QUEEN(circle QUEEN-SIZE "solid" "black"))
(define CELL(rectangle CELL-SIZE CELL-SIZE "outline" "red"))

(define(row n)
  (cond
    ((zero? n)empty-image)
    (else(beside CELL(row(sub1 n))))))

(define(board s)
  (local((define(h n)
           (cond
             ((zero? n)empty-image)
             (else(above(row s)(h(sub1 n)))))))
    (h s)))

(define(render-queens n queens image)
  (cond
    ((empty? queens)(board n))
    (else(place-image image
                      (+(* CELL-SIZE(posn-x(first queens)))HALF-CELL)
                      (+(* CELL-SIZE(posn-y(first queens)))HALF-CELL)
                      (render-queens n(rest queens)image)))))


(check-expect
 (render-queens 5(list(make-posn 2 2))QUEEN)
 (place-image QUEEN 
              (+(* CELL-SIZE 2)HALF-CELL)
              (+(* CELL-SIZE 2)HALF-CELL)
              (board 5)))


; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem 
 
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-1
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))

(define 4QUEEN-SOLUTION-2
  (list   (make-posn 1 0)(make-posn 0 2)
          (make-posn 3 1)(make-posn 2 3)))


(define 4QUEEN-SOLUTION-3
  (list  (make-posn 3 2) (make-posn 2 0)
         (make-posn 1 3) (make-posn 0 1)))

(define WRONG-SOLUTION
  (list  (make-posn 0 0) (make-posn 1 2)
         (make-posn 2 1) (make-posn 3 3)))
 

;; Design the n-queens-solution?

(check-satisfied 4QUEEN-SOLUTION-2(n-queens-solution? 4))
(check-satisfied 4QUEEN-SOLUTION-3(n-queens-solution? 4))
(check-expect((n-queens-solution? 3)4QUEEN-SOLUTION-2)#false)
(check-expect((n-queens-solution? 4)4QUEEN-SOLUTION-2)#true)
(check-expect((n-queens-solution? 5)4QUEEN-SOLUTION-2)#false)
(check-expect((n-queens-solution? 4)WRONG-SOLUTION)#false)
(check-expect((n-queens-solution? 5)WRONG-SOLUTION)#false)

(check-expect(all-safe? 4QUEEN-SOLUTION-2)#true)


(define(all-safe? solution)
  (cond
    ((empty? solution)#true)
    (else(and(andmap(lambda(x)(not(threatening? x(first solution))))
                    (rest solution))
             (all-safe?(rest solution))))))

(define(n-queens-solution? n)
  (lambda(solution)
    (and(=(length solution)n)
        (all-safe? solution))))


;; Design the function set=?.

(check-expect(set=?'()'())#true)
(check-expect(set=?'(1 2)'(2 1))#true)
(check-expect(set=?'(1 7 9)'(9 7 1))#true)
(check-expect(set=?'(1 7 9)'(3 7 9))#false)
(check-expect(set=? 4QUEEN-SOLUTION-1 4QUEEN-SOLUTION-2)#true)

(define(set=? one two)
  (and(=(length one)(length two))
      (local((define(h one two)
               (cond
                 ((empty? one)#true)
                 (else(and(member?(first one)two)
                          (h(rest one)two))))))
        (h one two))))

;; Exercise 482. The key idea to is to design a function

(define(create-row width y)
  (local((define(h width y)
           (cond
             ((zero? width)'())
             (else(cons(make-posn(sub1 width)y)
                       (h(sub1 width)y))))))
    (reverse(h width y))))


(define(all-positions n)
  (local((define(h c)
           (cond
             ((zero? c)'())
             (else(append(create-row n(sub1 c))
                         (h(sub1 c)))))))
    (reverse(h n))))

;; a Board collects those positions where a queen can still be placed;
(define(board0 n)
  (all-positions n))


;; a Board contains the list of positions where a queen has been placed;
(define(board1 n)
  '())


(define EMPTY-BOARD(board0 4))


(check-expect
 (create-row 4 2)
 (list(make-posn 0 2)(make-posn 1 2)(make-posn 2 2)(make-posn 3 2)))


(define(is-one-safe? QP QPS)
  (cond
    ((empty? QPS)#true)
    (else(and(not(threatening? QP(first QPS)))
             (is-one-safe? QP(rest QPS))))))


; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens initial-board n)
  (local((define(add-queen a-board qp)
           (cond
             ;; QUEENS
             ((empty? initial-board)(cons qp a-board))
             ;; GRID
             ((cons?(first initial-board))
              (cond
                ((empty? a-board)'())
                (else(cons(place-row(first a-board)qp)
                          (add-queen(rest a-board)qp)))))
             ;; NON-QUEENS
             (else(remove qp a-board))))
         (define(find-queens board)
           (cond
             ;; QUEENS
             ((empty? initial-board)board)
             ;; GRID
             ((cons?(first initial-board))
              (local((define(h c)
                       (cond
                         ((zero? c)'())
                         (else(append(find-open-spots board c)
                                     (h (sub1 c)))))))
                (h n)))
             ;; NON-QUEENS
             (else(filter(lambda(x)(not(member? x board)))initial-board))))
         (define(find-open-spots board index)
           (cond
             ;; QUEENS
             ((empty? initial-board)
              (filter (lambda(x)(is-one-safe? x(find-queens board)))(filter(lambda(x)(not(member? x(find-queens board))))(all-positions n))))
             ;; GRID
             ((cons?(first initial-board))
              (local((define ind(- n index)))
                (if(>= ind(length board))
                   '()
                   (map(lambda(x)(make-posn(squaree-x x)(squaree-y x)))(filter(lambda(x)(not(squaree-threatened x)))(list-ref board(- n index)))))))
             ;; NON-QUEENS
             (else(filter (lambda(x)(is-one-safe? x(find-queens board))) board))))
         (define(place board n)
           (cond
             ((zero? n)(find-queens board))
             (else
              (local((define open-spots(find-open-spots board n))
                     (define candidate(place-queens/list open-spots board n)))
                candidate))))
         (define(place-queens/list open-spots board index)
           (cond
             ((empty? open-spots)#false)
             (else(local((define candidate(place(add-queen board (first open-spots))(sub1 index))))
                    (cond
                      ((boolean? candidate)
                       (place-queens/list(rest open-spots)board index))
                      (else candidate)))))))
    (place initial-board n)))



(check-expect(place-queens(board0 3)3)#false)
(check-expect(length(place-queens(board0 4)4))4)
(check-satisfied(place-queens(board0 4)4)all-safe?)
(check-expect(length(place-queens(board0 5)5))5)
(check-satisfied(place-queens(board0 5)5)all-safe?)
(check-expect(length(place-queens(board0 6)6))6)
(check-satisfied(place-queens(board0 6)6)all-safe?)

(check-expect(place-queens(board1 3)3)#false)
(check-expect(length(place-queens(board1 4)4))4)
(check-satisfied(place-queens(board1 4)4)all-safe?)
(check-expect(length(place-queens(board1 5)5))5)
(check-satisfied(place-queens(board1 5)5)all-safe?)
(check-expect(length(place-queens(board1 6)6))6)
(check-satisfied(place-queens(board1 6)6)all-safe?)

(check-expect(place-queens(board2 3)3)#false)
(check-expect(length(place-queens(board2 4)4))4)
(check-satisfied(place-queens(board2 4)4)all-safe?)
(check-expect(length(place-queens(board2 5)5))5)
(check-satisfied(place-queens(board2 5)5)all-safe?)
(check-expect(length(place-queens(board2 6)6))6)
(check-satisfied(place-queens(board2 6)6)all-safe?)

;; too many queens
(check-expect(place-queens(board0 4)5)#false)
(check-satisfied(place-queens(board1 4)5)all-safe?);; board size doesnt matter for board1, only n is important
(check-expect(place-queens(board2 4)5)#false)


(check-expect
 (set=?(place-queens(board0 6)6)
       (place-queens(board1 6)6))
 #true)


;; ------------------------------------------------------------------------------------------------------------------------------------------------
;; ------------------------------------------------------------------------------------------------------------------------------------------------


(define-struct squaree [x y threatened])

(define(create-sq-row width y)
  (local((define(h width y)
           (cond
             ((zero? width)'())
             (else(cons(make-squaree(sub1 width)y #false)
                       (h(sub1 width)y))))))
    (reverse(h width y))))

;; a Board is a grid of n by n squares
(define(board2 n)
  (local((define(h c)
           (cond
             ((zero? c)'())
             (else(cons(create-sq-row n(sub1 c))
                         (h(sub1 c)))))))
    (reverse(h n))))


(define(place-row row pos)
  (cond
    ((empty? row)'())
    (else(cons
          (if(threatening? pos(make-posn(squaree-x(first row))(squaree-y(first row))))
             (make-squaree(squaree-x(first row))
                          (squaree-y(first row))
                          #true)
             (first row))
          (place-row(rest row)pos)))))



;; DONT THREAT QUEEN
