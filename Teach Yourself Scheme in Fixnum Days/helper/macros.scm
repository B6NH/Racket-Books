
(define-macro when
  (lambda (test . branch)
    (list 'if test (cons 'begin branch))))

(define-macro unless
  (lambda (test . branch)
    (list 'if (list 'not test) (cons 'begin branch))))

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
