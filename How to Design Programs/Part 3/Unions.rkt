;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Unions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define(all-even x)
  (zero?(modulo x 2)))

(define(all-odds x)
  (not(zero?(modulo x 2))))

(define(all-div-by-ten x)
  (zero?(modulo x 10)))

(define(union set1 set2)
  (lambda(x)(or(set1 x)(set2 x))))

(define(intersect set1 set2)
  (lambda(x)(and(set1 x)(set2 x))))

(define(add-element elem set)
  (lambda(x)(or(set x)(= x elem))))

#|
((union all-odds all-div-by-ten)11)
((union all-odds all-div-by-ten)90)
((add-element 99 all-even)98)
((intersect all-even all-div-by-ten)10)
|#