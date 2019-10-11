;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ranking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/web-io)

(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
     

(define (make-cell n)
  `(td ,(number->string n)))


(define (ranking los)
  (reverse (add-ranks (reverse los))))
 
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

(define(make-one-row songdata)
  (list 'tr
        (list (list 'width "200"))
        (list 'td(number->string (first songdata)))
        (list 'td (second songdata))))

(define(make-all-rows songs)
  (cond
    ((empty? songs)'())
    (else(cons(make-one-row(first songs))
              (make-all-rows(rest songs))))))
        

(define(make-ranking titles)
  `(html(body(table((border "1")),@(make-all-rows(ranking titles))))))

#|
(list
 'html
 (list
  'body
  (list
   'table
   (list (list 'border "1"))
   (list 'tr (list (list 'width "200")) (list 'td "1") (list 'td "2"))
   (list 'tr (list (list 'width "200")) (list 'td "99") (list 'td "65")))))


(list
 'html
 (list
  'body
  (list
   'table
   (list (list 'border "1"))
   (list
    (list 'tr (list (list 'width "200")) (list 'td "1") (list 'td "justyna"))
    (list 'tr (list (list 'width "200")) (list 'td "2") (list 'td "magda"))
    (list 'tr (list (list 'width "200")) (list 'td "3") (list 'td "paulina"))
    (list 'tr (list (list 'width "200")) (list 'td "4") (list 'td "ewa"))))))

|#


;;(show-in-browser(make-ranking one-list))


(check-expect
 (ranking one-list)
 '((1 "Asia: Heat of the Moment")
   (2 "U2: One")
   (3 "The White Stripes: Seven Nation Army")))

(check-expect
 (make-one-row(list 1 "Asia: Heat of the Moment"))
 (list 'tr 
       (list (list 'width "200"))
       (list 'td "1") 
       (list 'td "Asia: Heat of the Moment")))

(check-expect
 (make-ranking one-list)
 (list
  'html
  (list
   'body
  (list
   'table
   (list (list 'border "1"))
   (list
    'tr
    (list (list 'width "200"))
    (list 'td "1")
    (list 'td "Asia: Heat of the Moment"))
   (list 'tr (list (list 'width "200")) (list 'td "2") (list 'td "U2: One"))
   (list
    'tr
    (list (list 'width "200"))
    (list 'td "3")
    (list 'td "The White Stripes: Seven Nation Army"))))))

