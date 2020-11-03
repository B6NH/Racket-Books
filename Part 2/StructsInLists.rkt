;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname StructsInLists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

(define(wage wrk)
  (*(work-rate wrk)
    (work-hours wrk)))

(check-expect
 (wage(make-work (make-employee "John" 1234) 12.95 45))
 (* 12.95 45))


(define-struct employee[name number])

(define-struct paycheck[name number amount])

; Low -> List-of-numbers
; computes the weekly wages for the given records
(define (wage*.v4 an-low)
  (cond
    ((empty? an-low)'())
    (else(cons(make-paycheck(employee-name(work-employee(first an-low)))
                            (employee-number(work-employee(first an-low)))
                            (wage(first an-low)))
              (wage*.v4(rest an-low))))))



(define works(cons (make-work (make-employee "John" 1234) 12.95 45)
                   (cons (make-work (make-employee "Robby" 5678) 11.95 39)
                         '())))

(check-expect
 (wage*.v4 works)
 (cons(make-paycheck "John" 1234(* 12.95 45))(cons(make-paycheck "Robby" 5678(* 11.95 39)) '())))


(define(sum lop)
  (cond
    ((empty? lop)0)
    (else(+(posn-x(first lop))
           (sum(rest lop))))))

(check-expect
 (sum(list(make-posn 2 6)
          (make-posn 5 6)
          (make-posn 1 6)
          (make-posn 3 6)))
 11)

(define(translate lop)
  (cond
    ((empty? lop)'())
    (else(cons(make-posn(posn-x(first lop))
                        (+ 1(posn-y(first lop))))
              (translate(rest lop))))))


(check-expect
 (translate(list(make-posn 2 6)
                (make-posn 5 3)
                (make-posn 1 0)
                (make-posn 3 9)))
 (list(make-posn 2 7)
      (make-posn 5 4)
      (make-posn 1 1)
      (make-posn 3 10)))


(define(xlegal x)
  (and(> x 0)(< x 100)))

(define(ylegal x)
  (and(> x 0)(< x 200)))

(define(legal lop)
  (cond
    ((empty? lop)'())
    (else(if(and(xlegal(posn-x(first lop)))
                (ylegal(posn-y(first lop))))
            (cons(first lop)(legal(rest lop)))
            (legal(rest lop))))))

(check-expect(legal(list(make-posn 5 6)(make-posn 50 150)(make-posn 120 80)))
             (list(make-posn 5 6)(make-posn 50 150)))


(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999. 

(define(replace lop)
  (cond
    ((empty? lop)'())
    (else
     (cons
      (if(=(phone-area(first lop))713)
         (make-phone 281
                     (phone-switch(first lop))
                     (phone-four(first lop)))
         (first lop))
      (replace(rest lop))))))
           

(check-expect(replace(list(make-phone 123 456 7898)
                          (make-phone 713 777 8883)))
             (list(make-phone 123 456 7898)
                  (make-phone 281 777 8883)))
             




