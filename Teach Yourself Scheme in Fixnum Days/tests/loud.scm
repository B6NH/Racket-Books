
(define loud-null?
  (lambda (x)
    (display "Null check ")
    (write x) (newline)
    (null? x)))

(define loud-pair?
  (lambda (x)
    (display "Pair check ")
    (write x) (newline)
    (pair? x)))

(define tree->generator
  (lambda (tree)
    (let ((caller '*))
      (letrec
          ((generate-leaves
            (lambda ()
              (let loop ((tree tree))
                (display "Resuming ")
                (write tree)
                (cond ((loud-null? tree) 'skip)
                      ((loud-pair? tree)
                       (loop (car tree))
                       (loop (cdr tree)))
                      (else
                       (call/cc
                        (lambda (rest-of-tree)
                          (set! generate-leaves
                            (lambda ()
                              (rest-of-tree 'resume)))
                          (caller tree))))))
              (caller '()))))
        (lambda ()
          (call/cc
           (lambda (k)
             (set! caller k)
             (generate-leaves))))))))


(define mgen (tree->generator '(1 (2 3) (4 5))))

(display (mgen)) (newline)
(display "---\n")
(display (mgen)) (newline)
(display "---\n")
(display (mgen)) (newline)
(display "---\n")

