;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AlgoBack) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 29 Algorithms that Backtrack

#|
(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))
	
(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))
|#

;; Exercise 471. Translate one of the above definitions

(define sample-graph
  (list(list 'A (list 'B 'E))
       (list 'B (list 'E 'F))
       (list 'C (list 'D))
       (list 'D (list))
       (list 'E (list 'C 'F))
       (list 'F (list 'D 'G))
       (list 'G (list))))


; A Node is a Symbol.

(check-expect(neighbors 'A sample-graph)(list 'B 'E))
(check-error(neighbors 'Z sample-graph)"node not found")

(define(neighbors node graph)
  (cond
    ((empty? graph)(error "node not found"))
    (else
     (local((define fst(first graph)))
       (if(symbol=?(first fst)node)
          (second fst)
          (neighbors node(rest graph)))))))


; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 
 
; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
 
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
(check-expect(find-path 'A 'G sample-graph)
             (list 'A 'B 'E 'F 'G))
             
 
; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  (local((define (find-path/list lo-Os D)
           (local((define filtered
                    (filter(lambda(x)(not(boolean? x)))
                           (map(lambda(x)(find-path x D G))lo-Os))))
             (if(empty? filtered)#false(first filtered)))))
    (cond
      [(symbol=? origination destination) (list destination)]
      [else (local ((define next (neighbors origination G))
                    (define candidate
                      (find-path/list next destination)))
              (cond
                [(boolean? candidate) #false]
                [else (cons origination candidate)]))])))


;; Design test-on-all-nodes, a function that consumes a graph
(check-expect
 (tesg 'A sample-graph)
 (list
  (list 'A 'A #true)
  (list 'A 'B #true)
  (list 'A 'C #true)
  (list 'A 'D #true)
  (list 'A 'E #true)
  (list 'A 'F #true)
  (list 'A 'G #true)))
(check-expect
 (tesg 'C sample-graph)
 (list
  (list 'C 'A #false)
  (list 'C 'B #false)
  (list 'C 'C #true)
  (list 'C 'D #true)
  (list 'C 'E #false)
  (list 'C 'F #false)
  (list 'C 'G #false)))
             

(define(tesg node g)
  (local((define(h c)
           (cond
             ((empty? c)'())
             (else(cons(list node
                             (first(first c))
                             (not(boolean?(find-path node(first(first c))g))))
                       (h(rest c)))))))
    (h g)))
               
            
(define(test-on-all-nodes g)
  (local((define(h c)
           (cond
             ((empty? c)'())
             (else(append(tesg(first(first c))g)
                         (h(rest c)))))))
    (h g)))


(check-expect
 (length(test-on-all-nodes sample-graph))
 (sqr 7))


(define cyclic-graph
  (list(list 'A(list 'B 'E))
       (list 'B(list 'E 'F))
       (list 'C(list 'B 'D))
       (list 'D(list))
       (list 'E(list 'C'F))
       (list 'F(list 'D 'G))
       (list 'G(list))))


;; Exercise 476. Finite State Machines poses a problem concerning finite

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))


(check-expect(fsm-match? fsm-a-bc*-d "acbd")#true)
(check-expect(fsm-match? fsm-a-bc*-d "ad")#true)
(check-expect(fsm-match? fsm-a-bc*-d "abcd")#true)
(check-expect(fsm-match? fsm-a-bc*-d "abcbcd")#true)
(check-expect(fsm-match? fsm-a-bc*-d "abbd")#true)
(check-expect(fsm-match? fsm-a-bc*-d "d")#false)
(check-expect(fsm-match? fsm-a-bc*-d "da")#false)
(check-expect(fsm-match? fsm-a-bc*-d "aa")#false)
(check-expect(fsm-match? fsm-a-bc*-d "ab")#false)
(check-expect(fsm-match? fsm-a-bc*-d "abc")#false)
(check-expect(fsm-match? fsm-a-bc*-d "abdc")#false)


; FSM String -> Boolean 
; produces #true if the sequence of characters in the string causes the
; finite state machine to transition from an initial state to a final state
(define (fsm-match? an-fsm a-string)
  (local((define(find-new-state current-state key)
           (local((define result
                    (filter(lambda(x)(string? x))
                           (map(lambda(f)
                                 (if(and(string=? current-state(transition-current f))
                                        (string=? key(transition-key f)))
                                    (transition-next f)
                                    #false))
                               (fsm-transitions an-fsm)))))
             (if(empty? result)#false(first result))))
         (define(h current-state current-string)
           (cond
             ((empty? current-string)(string=? current-state(fsm-final an-fsm)))
             (else(local((define new-state
                           (find-new-state current-state
                                           (first current-string))))
                    (if(not(boolean? new-state))
                       (h new-state(rest current-string))
                       new-state))))))
    (h(fsm-initial an-fsm)(explode a-string))))


;; Exercise 477. Inspect the function definition of arrangements

; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
      (foldr (lambda (item others)
               (local ((define without-item
                         (arrangements (remove item w)))
                       (define add-item-to-front
                         (map (lambda (a) (cons item a))
                              without-item)))
                 (append add-item-to-front others)))
        '()
        w)]))
 
(define (all-words-from-rat? w)
  (and (member (explode "rat") w)
       (member (explode "art") w)
       (member (explode "tar") w)))
 
(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)
(check-expect
 (arrangements '("r" "a" "t"))
 (list
  (list "r" "a" "t")
  (list "r" "t" "a")
  (list "a" "r" "t")
  (list "a" "t" "r")
  (list "t" "r" "a")
  (list "t" "a" "r")))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements2 w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements2 (rest w)))]))


(define (insert-everywhere/in-all-words l alow)
  (cond
    [(empty? alow) empty]
    [else (append (insert-everywhere l (first alow))
                  (insert-everywhere/in-all-words l (rest alow)))]))

(define (insert-everywhere l a-word)
  (cond
    [(empty? a-word) (list(list l))]
    [else
     (cons (cons l a-word)
           (prefix-all (first a-word)
                       (insert-everywhere l (rest a-word))))]))


(define (prefix-all l words) 
  (cond
    [(empty? words) empty]
    [else (cons (cons l (first words))
                (prefix-all l (rest words)))]))



(check-expect
 (insert-everywhere "a" '("z" "r"))
 (list (list "a" "z" "r") (list "z" "a" "r") (list "z" "r" "a")))
(check-expect
 (insert-everywhere/in-all-words "a" '(("z" "r")("k" "c")))
 (list
  (list "a" "z" "r")
  (list "z" "a" "r")
  (list "z" "r" "a")
  (list "a" "k" "c")
  (list "k" "a" "c")
  (list "k" "c" "a")))

(check-satisfied (arrangements2 '("r" "a" "t"))
                 all-words-from-rat?)

(check-expect
 (arrangements2 '("r" "a" "t"))
 (list
  (list "r" "a" "t")
  (list "a" "r" "t")
  (list "a" "t" "r")
  (list "r" "t" "a")
  (list "t" "r" "a")
  (list "t" "a" "r")))


