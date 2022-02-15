
; Macros

(define a 5)
(define b 23)
(define c 50)
(define d 100)
(define e 0)

(define-macro my-when1
  (lambda (test . branch)
    (list 'if test
      (cons 'begin branch))))

(define-macro my-when2
  (lambda (test . branch)
    `(if ,test (begin ,@branch))))

(define-macro my-unless1
  (lambda (test . branch)
    (list 'if (list 'not test)
      (cons 'begin branch))))

(define-macro my-unless2
  (lambda (test . branch)
    (cons 'my-when1 (cons (list 'not test) branch))))

(define-macro my-or1
  (lambda (x y)
    `(if ,x ,x ,y)))

(define-macro my-or2
  (lambda (x y)
    `(let ((temp ,x))
       (if temp temp ,y))))

(define-macro my-or3
  (lambda (x y)
    (let ((temp (gensym)))
      `(let ((,temp ,x))
         (if ,temp ,temp ,y)))))

(define incrE
  (lambda ()
    (set! e (+ e 1))
    e))

(define-macro fluid-let

  ; List of variable-expression pairs and body
  (lambda (xexe . body)

    ; Variable names
    (let ((xx (map car xexe))

          ; Expressions
          (ee (map cadr xexe))

          ; New expression identifiers
          (old-xx (map (lambda (ig) (gensym)) xexe))

          ; New body identifier
          (result (gensym)))

      ; Save original values using new identifiers
      `(let ,(map (lambda (old-x x) `(,old-x ,x)) old-xx xx)

         ; Modify original values
         ,@(map (lambda (x e) `(set! ,x ,e)) xx ee)

            ; Evaluate body with modified values
            (let ((,result (begin ,@body)))

              ; Restore original values
              ,@(map (lambda (x old-x) `(set! ,x ,old-x)) xx old-xx)

              ; Return result
              ,result)))))


(begin

  (my-when1 (= a 5)
    (display "a = ") (display "five") (newline))

  (my-when2 (= d 100)
    (display "d = ") (display "hundred") (newline))

  (my-unless1 (< b 20)
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

