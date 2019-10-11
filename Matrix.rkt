;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define(identityM l)
  (local
    ((define(myfun n)
       (cond
         ((zero? n)'())
         (else(cons(create-for(sub1 n) l)(myfun(sub1 n)))))))
    (reverse(myfun l))))


(define(create-for index size)
  (cond
    ((zero? size)'())
    (else(cons(if(zero? index)1 0)
              (create-for (sub1 index)(sub1 size))))))



(define(identityM2 n)
  (build-list n(lambda(x)(create-for x n))))

;;(identityM 4)
;;(identityM2 4)