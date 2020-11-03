;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)


(define one
  (make-node
   15
   'd
   NONE
   (make-node
    24 'i NONE NONE)))

(define two
  (make-node
   15
   'd
   (make-node
    87 'h NONE NONE)
   NONE))


(define(contains-bt? num bt)
  (cond
    ((no-info? bt)#false)
    (else
     (or(= num(node-ssn bt))
        (contains-bt? num(node-left bt))
        (contains-bt? num(node-right bt))))))

(check-expect(contains-bt? 15 one)#true)
(check-expect(contains-bt? 17 one)#false)
(check-expect(contains-bt? 24 one)#true)
(check-expect(contains-bt? 15 two)#true)
(check-expect(contains-bt? 87 two)#true)
(check-expect(contains-bt? 3 two)#false)

;; ----------------------------------------------------------------
;; ----------------------------------------------------------------

(define(search-bt num bt)
  (cond
    ((not(contains-bt? num bt))#false)
    ((= num(node-ssn bt))(node-name bt))
    ((contains-bt? num(node-left bt))(search-bt num(node-left bt)))
    (else(search-bt num(node-right bt)))))


(check-expect(search-bt 15 one)'d)
(check-expect(search-bt 17 one)#false)
(check-expect(search-bt 24 one)'i)
(check-expect(search-bt 15 two)'d)
(check-expect(search-bt 87 two)'h)
(check-expect(search-bt 99 two)#false)

;; ----------------------------------------------------------------
;; ----------------------------------------------------------------

(define(inorder bst)
  (if(no-info? bst)'()
     (append
      (inorder(node-left bst))
      (list(node-ssn bst))
      (inorder(node-right bst)))))
  
(define example-bt
  (make-node 10 'a
             (make-node 20 'b
                        (make-node 60 'f NONE NONE)
                        NONE)
             (make-node 30 'c 
                        (make-node 40 'd NONE NONE)
                        (make-node 50 'e NONE NONE))))


(define example-bst
  (make-node 50 'a
             (make-node 25 'b
                        (make-node 15 'c NONE NONE)
                        (make-node 35 'd NONE NONE))
             (make-node 75 'e
                        (make-node 55 'f NONE NONE)
                        (make-node 80 'g NONE NONE))))


(define example-bst2
  (make-node 63 'a
             (make-node 29 'b
                        (make-node 15 'd
                                   (make-node 10 'h NONE NONE)
                                   (make-node 24 'i NONE NONE))
                        NONE)
             (make-node 89 'c
                        (make-node 77 'l NONE NONE)
                        (make-node 95 'g NONE (make-node 99 'o NONE NONE)))))

(check-expect(inorder example-bt)'(60 20 10 40 30 50))
(check-expect(inorder example-bst)'(15 25 35 50 55 75 80))

;; ----------------------------------------------------------------
;; ----------------------------------------------------------------

(define(search-bst num bst)
  (cond
    ((no-info? bst)NONE)
    ((= num(node-ssn bst))(node-name bst))
    ((< num(node-ssn bst))(search-bst num(node-left bst)))
    ((> num(node-ssn bst))(search-bst num(node-right bst)))))


(check-expect(search-bst 80 example-bst)'g)
(check-expect(search-bst 15 example-bst)'c)
(check-expect(search-bst 50 example-bst)'a)
(check-expect(search-bst 25 example-bst)'b)
(check-expect(search-bst 13 example-bst)NONE)


;; ----------------------------------------------------------------
;; ----------------------------------------------------------------


(define(create-bst bst num sym)
  (cond
    ((no-info? bst)(make-node num sym NONE NONE))
    (else
     (make-node
      (node-ssn bst)
      (node-name bst)
      (if(< num(node-ssn bst))
         (create-bst(node-left bst)num sym)
         (node-left bst))
      (if(>= num(node-ssn bst))
         (create-bst(node-right bst)num sym)
         (node-right bst))))))
        
    

(check-expect
 (create-bst example-bst2 13 'z)
 (make-node 63 'a
            (make-node 29 'b
                       (make-node 15 'd
                                  (make-node 10 'h NONE (make-node 13 'z NONE NONE))
                                  (make-node 24 'i NONE NONE))
                       NONE)
            (make-node 89 'c
                       (make-node 77 'l NONE NONE)
                       (make-node 95 'g NONE (make-node 99 'o NONE NONE)))))


(check-expect
 (create-bst example-bst2 120 'z)
 (make-node 63 'a
            (make-node 29 'b
                       (make-node 15 'd
                                  (make-node 10 'h NONE NONE)
                                  (make-node 24 'i NONE NONE))
                       NONE)
            (make-node 89 'c
                       (make-node 77 'l NONE NONE)
                       (make-node 95 'g NONE (make-node 99 'o NONE (make-node 120 'z NONE NONE))))))


;; ----------------------------------------------------------------
;; ----------------------------------------------------------------


;; [List-of [List Number Symbol]] -> BST
(define(create-bst-from-list lst)
  (cond
    ((empty? lst)NONE)
    (else(create-bst(create-bst-from-list(rest lst))
                    (first(first lst))
                    (second(first lst))))))

(define(create-bst-from-list2 lst)
  (foldr
   (lambda(x y)
     (create-bst y
      (first x)(second x)))NONE lst))

(define treedata'((99 o)(77 l)(24 i)(10 h)(95 g)(15 d)(89 c)(29 b)(63 a)))

(check-expect
 (create-bst-from-list
  treedata)
 example-bst2)

(check-expect
 (create-bst-from-list2
  treedata)
 example-bst2)

