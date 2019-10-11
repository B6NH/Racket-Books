;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname NestedXml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; Are you wondering whether arbitrary nesting is the correct way

(define SIZE 12) ; font size 
(define COLOR "black") ; font color 
(define BT ; a graphical constant 
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))


; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BT item))

#|
 An XItem1 is one of: 
 – (cons 'li (cons XWord '()))
 – (cons 'li (cons [List-of Attribute] (list XWord)))

 An XItem2 is one of: 
 – (cons 'li (cons XWord '()))
 – (cons 'li (cons [List-of Attribute] (list XWord)))
 – (cons 'li (cons XEnum1 '()))
 – (cons 'li (cons [List-of Attribute] (list XEnum1)))

 An XItem3 is one of: 
 – (cons 'li (cons XWord '()))
 – (cons 'li (cons [List-of Attribute] (list XWord)))
 – (cons 'li (cons XEnum2 '()))
 – (cons 'li (cons [List-of Attribute] (list XEnum2)))
 
 An XEnum3 is one of:
 – (cons 'ul [List-of XItem3])
 – (cons 'ul (cons [List-of Attribute] [List-of XItem3]))

 An XEnum2 is one of:
 – (cons 'ul [List-of XItem2])
 – (cons 'ul (cons [List-of Attribute] [List-of XItem2]))

 An XEnum1 is one of:
 – (cons 'ul [List-of XItem1])
 – (cons 'ul (cons [List-of Attribute] [List-of XItem1]))
|#

(define(xexpr-name xe)
  (first xe))

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

(define(find-attr loa symbol)
  (local((define result(assq symbol loa)))
    (if(boolean? result)result(second result))))

(define(word-text aword)
  (find-attr(xexpr-attr aword)'text))

(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

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

(define example-item-1(list 'li(list 'word(list(list 'text "one")))))
(define example-enum-1(list 'ul example-item-1))
(define example-item-2(list 'li example-enum-1))
(define example-enum-2(list 'ul example-item-2))
(define example-item-3(list 'li example-enum-2))
(define example-enum-3(list 'ul example-item-3))


(define (render-item1 an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (text (word-text content) SIZE 'black))))

(define (render-item2 an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(word? content)
         (text (word-text content) SIZE 'black)]
        [else (render-enum1 content)]))))


(define (render-item3 an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(word? content)
         (text (word-text content) SIZE 'black)]
        [else (render-enum2 content)]))))


(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          (define (deal-with-one item so-far)
            (above/align 'left (render-item1 item) so-far)))
    (foldr deal-with-one empty-image content)))
(define (render-enum2 xe)
  (local ((define content (xexpr-content xe))
          (define (deal-with-one item so-far)
            (above/align 'left (render-item2 item) so-far)))
    (foldr deal-with-one empty-image content)))
(define (render-enum3 xe)
  (local ((define content (xexpr-content xe))
          (define (deal-with-one item so-far)
            (above/align 'left (render-item3 item) so-far)))
    (foldr deal-with-one empty-image content)))

(render-enum3 example-enum-3)


        