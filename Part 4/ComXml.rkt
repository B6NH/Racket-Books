;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ComXml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
;; 22 Project: The Commerce of XML

(define ex1'(machine))
(define ex2'(machine (action)))
(define ex3'(machine (action) (action) (action)))
(define ex4'(machine ((initial "red"))))
(define ex5'
  (machine
   ((initial "red"))
   (action ((state "red") (next "green")))
   (action ((state "green") (next "yellow")))
   (action ((state "yellow") (next "red")))))

; An Xexpr.v2 is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))


;; Exercise 364
(define ans1
  '(transition
    ((from "seen-e")
     (to "seen-f"))))

(define ans2
  '(ul(li(word)(word))
      (li(word))))

;; Exercise 365
;; ans 1 - <server name="example.org" />
;; ans 2 - <carcas> <board><grass></grass></board> <player name="sam"></player></carcas>
;; ans 3 - <start></start>

(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(check-expect (xexpr-name e0) 'machine)

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action)(action)))

(check-expect (find-attr '((initial "A")(middle "B"))'middle)"B")
(check-expect (find-attr '((initial "A")(middle "B"))'initial)"A")
(check-expect (find-attr '((initial "A")(middle "B"))'other)#false)

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define(xexpr-attr xe)
  (local((define optional-loa+content(rest xe)))
    (cond
      [(empty? optional-loa+content)'()]
      [else
       (local((define loa-or-x
                (first optional-loa+content)))
         (if(list-of-attributes? loa-or-x)
            loa-or-x
            '()))])))


;; Exercise 366. Design xexpr-name and xexpr-content.

(define(xexpr-name xe)
  (first xe))

(define(xexpr-content xe)
  (local((define optional-loa+content(rest xe)))
    (cond
      ((empty? optional-loa+content)'())
      (else
       (local((define loa-or-x
                (first optional-loa+content)))
         (if(list-of-attributes? loa-or-x)
            (rest optional-loa+content)
            optional-loa+content))))))

;; Exercise 367. The design recipe calls
;; function needs only one list of attributes

;; Exercise 368. Formulate a data definition
;; List of lists

;; Exercise 369. Design find-attr
(define(find-attr loa symbol)
  (local((define result(assq symbol loa)))
    (if(boolean? result)result(second result))))


;; 22.2 Rendering XML Enumerations

;; Exercise 370. Make up three

(define xword1 '(word((text "tea"))))
(define xword2 '(word((text "water"))))
(define xword3 '(word((text "summer"))))

(check-expect(word? xword2)#true)
(check-expect(word? 12)#false)
(check-expect(word?'())#false)
(check-expect(word?'(word((text "summer"))))#true)
(check-expect(word?'(word((text "summer")(color "black"))))#false)
(check-expect(word?'(asd((text "summer"))))#false)
(check-expect(word?'(word(("text" "summer"))))#false)
(check-expect(word?'(word((text summer))))#false)
(check-expect(word?'("word"((text summer))))#false)
(check-expect(word?'("word"))#false)
(check-expect(word?'(word))#false)

(check-expect(word-text xword3)"summer")

(define(word? value)
  (and
   (cons? value)
   (=(length value)2)
   (local((define name(xexpr-name value)))
     (and(symbol? name)
         (symbol=? name 'word)
         (local((define attributes(xexpr-attr value)))
           (and
            (=(length attributes)1)
            (string?(find-attr attributes 'text))))))))

(define(word-text aword)
  (find-attr(xexpr-attr aword)'text))


; An XEnum.v1 is one of:
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))

;; example item
(define example-item '(li (word ((text "one")))))

(define example-item-rendered
  (beside/align 'center BT (text "one" 12 'black)))


(define (render-item1 i)
  (local ((define content (xexpr-content i));; (list (list 'word (list (list 'text "one"))))
          (define element (first content));; (list 'word (list (list 'text "one")))
          (define a-word (word-text element));; "one"
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

;; Exercise 372. Before you
(check-expect(render-item1 example-item)example-item-rendered)


;; example enum
(define new-e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define new-e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

(check-expect (render-enum1 new-e0) new-e0-rendered)

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left
                         (render-item1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))


; An XItem.v2 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
;
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; Image -> Image
; marks item with bullet
(define (bulletize item)
  (beside/align 'center BT item))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else(render-enum content)]))))

;; Exercise 373. Figure 128 is missing test


(define hello-xitem-1
  (cons 'li(cons'(word((text "hello")))'())))
(define hello-xitem-2
  (cons 'li(cons'((color "red")(padding 2))
                (list'(word((text "BBB")))))))
(define simple-xenum(cons 'ul(list hello-xitem-1 hello-xitem-2)))
(define hello-xitem-3(cons 'li(cons simple-xenum '())))
(define hello-xitem-4(cons 'li(cons'((color "purple")
                                     (margin 5))(cons simple-xenum '()))))

(define advanced-xenum
  (cons 'ul(cons'((color "red")(padding 12))
                (list hello-xitem-1
                      hello-xitem-2
                      hello-xitem-3))))


(check-expect
 (render-item hello-xitem-1)
 (beside/align 'center BT(text "hello" SIZE 'black)))

(check-expect
 (render-item hello-xitem-2)
 (beside/align 'center BT(text "BBB" SIZE 'black)))

(check-expect
 (render-enum simple-xenum)
 (above/align 'left
              (beside/align 'center BT(text "hello" SIZE 'black))
              (above/align 'left
                           (beside/align 'center BT(text "BBB" SIZE 'black))
                           empty-image)))

(check-expect
 (render-item hello-xitem-3)
 (beside/align 'center BT
               (above/align 'left
                            (beside/align 'center BT(text "hello" SIZE 'black))
                            (above/align 'left
                                         (beside/align 'center BT(text "BBB" SIZE 'black))
                                         empty-image))))

(check-expect
 (render-enum advanced-xenum)
 (above/align 'left(beside/align 'center BT(text "hello" SIZE 'black))
              (above/align 'left
                           (beside/align 'center BT(text "BBB" SIZE 'black))
                           (beside/align 'center BT
                                         (above/align 'left
                                                      (beside/align 'center BT(text "hello" SIZE 'black))
                                                      (above/align 'left
                                                                   (beside/align 'center BT(text "BBB" SIZE 'black))
                                                                   empty-image))))))


;; Exercise 374. The data definitions

; An XItem.v2 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))
;
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))



;; Exercise 376. Design a program that counts

(define (count-hello-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (cond
      [(word? content)
       (if(string=?(word-text content)"hello")1 0)]
      [else(count-hello-enum content)])))


(define (count-hello-enum xe)
  (local ((define content (xexpr-content xe))
          (define (deal-with-one item so-far)
            (+(count-hello-item item) so-far)))
    (foldr deal-with-one 0 content)))

(check-expect(count-hello-item hello-xitem-1)1)
(check-expect(count-hello-item hello-xitem-2)0)
(check-expect(count-hello-item hello-xitem-3)1)
(check-expect(count-hello-enum simple-xenum)1)
(check-expect(count-hello-enum advanced-xenum)2)

;; Exercise 377. Design a program that replaces


(define (replace-hello-item an-item)
  (local ((define content (first (xexpr-content an-item)))
          (define attrs(xexpr-attr an-item)))
    (cons 'li
          (local
            ((define result
               (cond
                 [(word? content)
                  (local((define text(word-text content))
                         (define newtext(if(string=? "hello" text)"bye" text)))
                    (cons`(word((text ,newtext)))'()))]
                 [else
                  (cons(replace-hello-enum content)'())])))
            (if(empty? attrs)result(cons attrs result))))))


(define (replace-hello-enum xe)
  (local ((define content (xexpr-content xe))
          (define attrs (xexpr-attr xe))
          (define (deal-with-one item so-far)
            (cons(replace-hello-item item) so-far)))
    (cons 'ul
          (if(empty? attrs)
             (foldr deal-with-one '() content)
             (cons attrs(foldr deal-with-one '() content))))))


(check-expect
 (replace-hello-item hello-xitem-1)
 (cons 'li(cons'(word((text "bye")))'())))


(check-expect
 (replace-hello-item hello-xitem-2)
 (cons 'li(cons'((color "red")(padding 2))
               (list'(word((text "BBB")))))))

(check-expect
 (replace-hello-enum advanced-xenum)
 (list
  'ul
  (list (list 'color "red") (list 'padding 12))
  (list 'li (list 'word (list (list 'text "bye"))))
  (list
  'li
  (list (list 'color "red") (list 'padding 2))
  (list 'word (list (list 'text "BBB"))))
  (list
   'li
   (list
    'ul
    (list 'li (list 'word (list (list 'text "bye"))))
    (list
     'li
     (list (list 'color "red") (list 'padding 2))
     (list 'word (list (list 'text "BBB"))))))))

(check-expect
 (replace-hello-item hello-xitem-4)
 (list
  'li
  (list (list 'color "purple") (list 'margin 5))
  (list
   'ul
   (list 'li (list 'word (list (list 'text "bye"))))
   (list
   'li
   (list (list 'color "red") (list 'padding 2))
   (list 'word (list (list 'text "BBB")))))))


;; 22.3 Domain-Specific Languages

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color

; data examples(third item is proper key)
(define fsm-traffic
  '(("red" "green" "r") ("green" "yellow" "g") ("yellow" "red" "y")))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))


; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay(text current 14 "black")(square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (local((define new-current(find transitions current)))
          (if(key=?(third(assoc current transitions))key-event)
             new-current
             current)))]))


;; Exercise 379. Formulate test cases for find.

(check-expect(find fsm-traffic "red")"green")
(check-expect(find fsm-traffic "green")"yellow")
(check-expect(find fsm-traffic "yellow")"red")
(check-error(find fsm-traffic "blue"))

(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green") (key "r")))
     (action ((state "green") (next "yellow")(key "g")))
     (action ((state "yellow") (next "red")(key "y")))))

;; Exercise 381. The definitions of XMachine and X1T use quote

; An XMachine is a nested list of this shape:
; (list machine (list(list initial FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
; (list action (list(list state FSM-State) (list next FSM-State)))

; An XMachine is a nested list of this shape:
; (cons machine(cons(cons(cons initial(cons FSM-State '()))'())  (cons [List-of X1T] '())))
; An X1T is a nested list of this shape:
; (cons action(cons(cons(cons state(cons FSM-State '()))  (cons(cons next(cons FSM-State '()))'()))'()))


;; Exercise 382. Formulate an XML

#|
<machine initial="black">

  <action state="black"    next="white" />

  <action state="white"  next="black" />

</machine>
|#

(define bw-machine
  '(machine
    ((initial "black"))
    (action((state "black")(next "white")(key "b")))
    (action((state "white")(next "black")(key "w")))))


(check-expect (xm-state0 xm0) "red")
(check-expect (xm->transitions xm0) fsm-traffic)

(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))

(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next)
                  (find-attr (xexpr-attr xa) 'key))))
    (map xaction->action (xexpr-content xm))))


; XMachine -> FSM-State
; interprets the given configuration as a state machine
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

;; 22.4 Reading XML

(define(get-xexpr x s)
  (local((define attributes(xexpr-attr x))
         (define itemprop(find-attr attributes 'itemprop))
         (define content(find-attr attributes 'content)))
    (if(and(string? itemprop)
           (string=? itemprop s))
       content
       #false)))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")

(check-error
 (get '(meta ((content "+1") (itemprop "F"))) "Z"))

(check-expect(get-xexpr'(meta ((content "+1") (itemprop "F"))) "F")"+1")
(check-expect(get-xexpr'(meta ((content "+1") (itemprop "F"))) "Z")#false)

(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))


;; 23 Simultaneous Processing
