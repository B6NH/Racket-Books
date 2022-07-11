(include "helper/macros.scm")

; Macros

(define a 5)
(define b 23)
(define c 50)
(define d 100)
(define e 0)

; Macro template
; Comma inserts result of evaluating expression
; Comma-splice inserts result of evaluating
; expression after removing outermost parentheses
(define-macro my-when2
  (lambda (test . branch)
    `(if ,test (begin ,@branch))))

(define-macro my-unless2
  (lambda (test . branch)
    (cons 'when (cons (list 'not test) branch))))

; Double evaluation
(define-macro my-or1
  (lambda (x y)
    `(if ,x ,x ,y)))

; Single evaluation
(define-macro my-or2
  (lambda (x y)
    `(let ((temp ,x))
       (if temp temp ,y))))

; Generate unique identifier for variable x
(define-macro my-or3
  (lambda (x y)
    (let ((temp (gensym)))
      `(let ((,temp ,x))
         (if ,temp ,temp ,y)))))

(define incrE
  (lambda ()
    (set! e (+ e 1))
    e))

(begin

  (when (= a 5)
    (display "a = ") (display "five") (newline))

  (my-when2 (= d 100)
    (display "d = ") (display "hundred") (newline))

  (unless (< b 20)
    (display "b >= ") (display "20 ") (newline))

  (my-unless2 (> c 60)
    (display "c <= ") (display "60 ") (newline))

  (fluid-let ((e e))
    (display (incrE)) (newline)
    (display (incrE)) (newline)
    (display (incrE)) (newline))

  (display
    (and (= (my-or1 5 6) 5)
         (= (my-or2 #f 8) 8)
         (= (my-or3 9 #f) 9)
         (= 0 e)))

  (newline))

