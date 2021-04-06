;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Scoa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
;; Intermezzo 3: Scope and Abstraction

;; ISL for Loops

(define (enumerate lx)
  (for/list ([x lx] [ith (length lx)])
    (list (+ ith 1) x)))

(define(enumeratef lx)
  (local((define nums
           (build-list(length lx)(lambda(z)(+ z 1))))
         (define(helper indexes lst)
           (cond
             ((empty? indexes)'())
             (else(cons(cons(first indexes)(cons(first lst)'()))
                       (helper(rest indexes)(rest lst)))))))
    (helper nums lx)))


(check-expect
 (enumerate '(a b c)) '((1 a) (2 b) (3 c)))

(check-expect
 (enumeratef '(a b c)) '((1 a) (2 b) (3 c)))


;;(for/list ([i 2] [j '(a b)]) (list i j))
;;(for*/list ([i 2] [j '(a b)]) (list i j))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generates all pairs of items from l1 and l2

(check-satisfied (cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))

(define (cross l1 l2)
   (for*/list ([x1 l1][x2 l2])
      (list x1 x2)))


;; Pattern Matching

(define-struct phone [area switch four])

(define (replace records)
  (for/list ((p records))
    (match p
      [(phone 713 y z) (make-phone 281 y z)]
      [(phone x y z) (make-phone x y z)])))

(check-expect
 (replace(list(make-phone 10 20 30)
              (make-phone 713 90 56)))
 (list(make-phone 10 20 30)
      (make-phone 281 90 56)))


;; determines number of strings per item in a list of list of strings
(define(words-on-line lols)
  (for/list((row lols))
    (local((define(procrow somerow)
             (match somerow
               ['() 0]
               [(cons head tail)(+ 1(procrow tail))])))
      (procrow row))))

(check-expect
 (words-on-line(list(list "a" "b" "c")'()(list "a" "b")))
 (list 3 0 2))
