;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Accu) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; VI Accumulators

;; 31 The Loss of Knowledge

; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin

(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))

 
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))


;; Exercise 490. Develop a formula
;; n add-to-each recursions, n+1 relative->absolute recursions
 
; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
 

(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))

 
(define (add-to-each n l)
  (map(lambda(x)(+ x n))l))


; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute.v2 '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute.v2 l0)
  (local (
    ; [List-of Number] Number -> [List-of Number]
    (define (relative->absolute/a l accu-dist)
      (cond
        [(empty? l) '()]
        [else
          (local ((define accu (+ (first l) accu-dist)))
            (cons accu
                 (relative->absolute/a (rest l) accu)))])))
    (relative->absolute/a l0 0)))


;; Exercise 491. With a bit of design and a bit of tinkering


(define (relative->absoluteg l)
 (reverse
   (foldr (lambda (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse (rest l)))))

(check-expect (relative->absoluteg '(50 40 70 30 30))
              '(50 90 160 190 220))


(define(addlast item lst)
  (cond
    ((empty? lst)(list item))
    (else(cons(first lst)
              (addlast item(rest lst))))))

(check-expect(addlast 5'(4 1 3))
             '(4 1 3 5))

(check-expect(addlast 5'())
             '(5))

(define(reverseg lst)
  (cond
    ((empty? lst)'())
    (else(addlast(first lst)
                 (reverseg(rest lst))))))

(define(reversef lst)
  (foldl(lambda(x y)(cons x y))'()lst))

(check-expect(reverseg '())'())
(check-expect(reverseg '(1 2 3 4))'(4 3 2 1))
(check-expect(reversef '())'())
(check-expect(reversef '(1 2 3 4))'(4 3 2 1))


;; 31.2 A Problem with Generative Recursion

(define a-sg
  '((A B)
    (B C)
    (C E)
    (D E)
    (E B)
    (F F)))


; Node Node SimpleGraph -> Boolean
; is there a path from origin to destination in sg
 
(check-expect (path-exists.v2? 'A 'E a-sg) #true)
(check-expect (path-exists.v2? 'A 'F a-sg) #false)
 
(define (path-exists.v2? origin destination sg)
  (local (; Node Node SimpleGraph [List-of Node] -> Boolean
          (define (path-exists?/a origin seen)
            (cond
              [(symbol=? origin destination) #t]
              [(member? origin seen) #f]
              [else (path-exists?/a (neighbor origin sg)
                                    (cons origin seen))])))
    (path-exists?/a origin '())))


 
; Node SimpleGraph -> Node
; determine the node that is connected to a-node in sg
(check-expect (neighbor 'A a-sg) 'B)
(check-error (neighbor 'G a-sg) "neighbor: not a node")
(define (neighbor a-node sg)
  (cond
    [(empty? sg) (error "neighbor: not a node")]
    [else (if (symbol=? (first (first sg)) a-node)
              (second (first sg))
              (neighbor a-node (rest sg)))]))


;; Exercise 492. Modify the definitions in figure 169 so that

(define(neighbors node graph)
  (cond
    ((empty? graph)(error "node not found"))
    (else
     (local((define fst(first graph)))
       (if(symbol=?(first fst)node)
          (second fst)
          (neighbors node(rest graph)))))))

(define cyclic-graph
  (list(list 'A(list 'B 'E))
       (list 'B(list 'E 'F))
       (list 'C(list 'B 'D))
       (list 'D(list))
       (list 'E(list 'C'F))
       (list 'F(list 'D 'G))
       (list 'G(list))))


; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  (local((define(fp origination destination seen)
           (cond
             [(member? origination seen)#false]
             [(symbol=? origination destination) (list destination)]
             [else (local ((define next (neighbors origination G))
                           (define candidate
                             (find-path/list next destination(cons origination seen))))
                     (cond
                       [(boolean? candidate) #false]
                       [else (cons origination candidate)]))]))
         (define (find-path/list lo-Os D seen)
           (cond
             [(empty? lo-Os) #false]
             [else (local ((define candidate
                             (fp (first lo-Os) D seen)))
                     (cond
                       [(boolean? candidate)
                        (find-path/list (rest lo-Os) D seen)]
                       [else candidate]))])))
    (fp origination destination '())))


(check-expect
 (find-path 'A 'E cyclic-graph)
 (list 'A 'B 'E))

(check-expect
 (find-path 'A 'C cyclic-graph)
 (list 'A 'B 'E 'C))

(check-expect
 (find-path 'D 'G cyclic-graph)
 #false)

(check-expect
 (find-path 'A 'G cyclic-graph)
 (list 'A 'B 'E 'F 'G))

(check-expect
 (find-path 'C 'F cyclic-graph)
 (list 'C 'B 'E 'F))

(check-error
 (find-path 'Z 'M cyclic-graph)
 "node not found")


;; Exercise 494. Does the insertion sort> function

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))


(define(sortv l)
  (foldl(lambda(x y)(insert x y))'()l))


(check-expect(sortv'(1 2 3 4 5 6 7 8))(sort>'(1 2 3 4 5 6 7 8)))


#|
(define aaa(time(reverse(build-list 5000 add1))))
(define ccc(time(reverseg(build-list 5000 add1))));; with addlast
(define zzz(time(reversef(build-list 5000 add1))));; with accumulator
|#


;; 32.3 Transforming Functions into Accumulator Style

(check-expect(sum.v1'(1 2 3 4 5))15)
(check-expect(sum.v2'(1 2 3 4 5))15)


(define (sum.v1 alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum.v1 (rest alon)))]))


(define (sum.v2 alon0)
  (local (; [List-of Number] ??? -> Number
          ; computes the sum of the numbers on alon
          ; accumulator ...
          (define (sum/a alon a)
            (cond
              [(empty? alon) a]
              [else (sum/a (rest alon)
                     (+(first alon)a))])))
    (sum/a alon0 0)))


;; For the second example, we turn to the well-known factorial function:

; N -> N 
; computes (* n (- n 1) (- n 2) ... 1)
(check-expect (!.v1 3) 6)
(check-expect (!.v2 3) 6)
(check-expect (!.v1 5) 120)
(check-expect (!.v2 5) 120)

(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))


(define (!.v2 n0)
  (local ((define (!/a n a)
            (cond
              [(zero? n)a]
              [else (!/a (sub1 n)
                         (* n a))])))
    (!/a n0 1)))


;; Here are the relevant definitions: 


(check-expect(height '())0)
(check-expect(height(make-node '()'()))1)
(check-expect(height(make-node (make-node '() '())'()))2)
(check-expect(height(make-node (make-node '() (make-node '() '()))(make-node '() '())))3)

(check-expect(height.v2 '())0)
(check-expect(height.v2(make-node '()'()))1)
(check-expect(height.v2(make-node (make-node '() '())'()))2)
(check-expect(height.v2(make-node (make-node '() (make-node '() '()))(make-node '() '())))3)



(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))


(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt))) 1)]))

; Tree -> N
; measures the height of abt0
(check-expect (height.v2 example) 3)

(define (height.v2 abt0)
  (local (; Tree N -> N
          ; measures the height of abt
          ; accumulator a is the number of steps 
          ; it takes to reach abt from abt0
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
               (max
                 (height/a (node-left abt)  (+ a 1))
                 (height/a (node-right abt) (+ a 1)))])))
    (height/a abt0 0)))



;; Exercise 498. Complete height.v3. Hint The bottom-most tree


(check-expect(height.v3 '())0)
(check-expect(height.v3(make-node '()'()))1)
(check-expect(height.v3(make-node (make-node '() '())'()))2)
(check-expect(height.v3(make-node (make-node '() (make-node '() '()))(make-node '() '())))3)
(check-expect(height.v3 example)(height example))

(define(height.v3 abt)
  (local((define (h/a abt s m)
           (cond
             [(empty? abt)(max s m)]
             [else
              (h/a(node-right abt)
                  (add1 s)
                  (h/a(node-left abt)(add1 s)m))])))
    (h/a abt 0 0)))




;; Exercise 499. Design an accumulator-style version of product

(check-expect(product'(4 2 3 1 5))120)
(check-expect(product'(4 2 0 1 5))0)

(define(product lst)
  (local((define(h nums acc)
           (cond
             ((empty? nums)acc)
             (else(h(rest nums)
                    (*(first nums)acc))))))
    (h lst 1)))


;; Exercise 500. Design an accumulator-style version of how-many

(check-expect(how-many'(4 2 3 1 5))5)
(check-expect(how-many'(4 2 7))3)
(check-expect(how-many'())0)

(define(how-many lst)
  (local((define(h lst a)
           (cond
             ((empty? lst)a)
             (else(h(rest lst)(add1 a))))))
    (h lst 0)))


;; Exercise 501. Design an accumulator-style version of add-to-pi

; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))


(check-within (add-to-pi-v2 2) (+ 2 pi) 0.001)

(define(add-to-pi-v2 n)
  (local((define(h n acc)
           (cond
             ((zero? n)acc)
             (else(h(sub1 n)(add1 acc))))))
    (h n pi)))


;; Exercise 502. Design the function palindrome, which accepts a non-empty list

(check-expect(all-but-last '(5 2 3 1 4 2))'(5 2 3 1 4))
(check-expect(last '(5 2 3 1 4 2))2)

(define(last lst)
  (cond
    ((empty?(rest lst))(first lst))
    (else(last(rest lst)))))


(define(all-but-last lst)
  (cond
    ((empty?(rest lst))'())
    (else(cons(first lst)
              (all-but-last(rest lst))))))

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
  (mirror (explode "abc")) (explode "abcba"))
(define (mirror s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))


(check-expect
  (mirror-v2 (explode "abc")) (explode "abcba"))

(check-expect
  (mirror-v2 (explode "grts")) (explode "grtstrg"))


(define(mirror-v2 lst)
  (local((define(h la a)
           (cond
             ((empty?(rest la))(append lst a))
             (else(h(rest la)(cons(first la)a))))))
    (h lst '())))


;; Exercise 503. Exercise 467 implicitly asks for the design of a function that rotates a Matrix


; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))

(check-error(rotate '((0 4 5) (0 2 3)))"all zeros!")


(define zeros'(0 0 0))

(define big-matrix(addlast '(1 0 0)(build-list 5000(lambda(x)zeros))))


(define (rotate M)
  (if(andmap(lambda(x)(zero?(first x)))M)
     (error "all zeros!")
     (cond
       [(not (= (first (first M)) 0)) M]
       [else
        (rotate (append (rest M) (list (first M))))])))


(define (rotate.v2 M0)
  (local ((define (rotate/a M seen)
            (cond
              ((not(zero?(first(first M))))(append M seen))
              (else(rotate/a(rest M)(cons(first M)seen))))))
    (rotate/a M0 '())))

(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))


(define cat(time(rotate big-matrix)))
(define cut(time(rotate.v2 big-matrix)))

(check-expect cat cut)



;; Exercise 504. Design to10. It consumes a list

(check-expect(to10'(1 0 2))102)
(check-expect(to10'(5 4))54)
(check-expect(to10'(6 2 3))623)
(check-expect(to10'(0 2 3))23)

(define(to10 lst)
  (local((define(h lst n acc)
           (cond
             ((empty? lst)acc)
             (else(h(rest lst)(sub1 n)(+(*(first lst)(expt 10 n)) acc))))))
    (h lst(sub1(length lst)) 0)))


;; Exercise 505. Design the function is-prime, which consumes

(check-expect(is-prime 2)#true)
(check-expect(is-prime 7)#true)
(check-expect(is-prime 13)#true)
(check-expect(is-prime 59)#true)
(check-expect(is-prime 199)#true)
(check-expect(is-prime 60)#false)
(check-expect(is-prime 65)#false)
(check-expect(is-prime 200)#false)

(define(is-prime n)
  (local((define(h c a)
           (cond
             ((= 1 c)#true)
             ((zero?(remainder a c))#false)
             (else(h(sub1 c)a)))))
    (h(sub1 n)n)))



;; Exercise 506. Design an accumulator-style version of map


(define(accumap f lst)
  (local((define(h lst acc)
           (cond
             ((empty? lst)acc)
             (else(h(rest lst)
                    (cons(f(first lst))acc))))))
    (h (reverse lst) '())))


(check-expect
 (accumap add1'(2 5 4 2 3 1))'(3 6 5 3 4 2))
                      

;; Exercise 507. Exercise 257 explains how to design foldl

(define t6(lambda(x)(* x 6)))

(check-expect (build-l*st 10 add1) (build-list 10 add1))
(check-expect (build-l*st 10 t6) (build-list 10 t6))


(define(build-l*st s f)
  (local((define(h c acc)
           (cond
             ((< c 0)acc)
             (else
              (h(sub1 c)(cons(f c)acc))))))
    (h(sub1 s)'())))

