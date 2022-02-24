
; Jumps

; Escaping continuation
(display
  (+ 1 (call/cc
         (lambda (k)
           (+ 2 (k 3))))))
(newline)

; Save continuation in variable r
(define r #f)
(display
  (+ 7 (call/cc
         (lambda (k)
           (set! r k)
           (+ 2 (k 3))))))
(newline)

; Multiply list of numbers
(define list-product1
  (lambda (s)
    (let recur ((s s))
      (if (null? s) 1
          (* (car s) (recur (cdr s)))))))

; Use continuation to quit function when number 0 is encountered
(define list-product2
  (lambda (s)
    (call/cc
      (lambda (exit)
        (let recur ((s s))
          (if (null? s)
              1
              (if (= (car s) 0)
                  (exit 0)
                  (* (car s) (recur (cdr s))))))))))

(define flatten
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((pair? (car tree))
        (append (flatten (car tree))
                (flatten (cdr tree))))
      (else
        (cons (car tree)
              (flatten (cdr tree)))))))

(define same-fringe1?
  (lambda (tree1 tree2)
    (let loop ((ftree1 (flatten tree1))
               (ftree2 (flatten tree2)))
      (cond
        ((and (null? ftree1) (null? ftree2)) #t)
        ((or (null? ftree1) (null? ftree2)) #f)
        ((eqv? (car ftree1) (car ftree2))
         (loop (cdr ftree1) (cdr ftree2)))
        (else #f)))))

(define same-fringe2?
  (lambda (tree1 tree2)
    (let ((gen1 (tree->generator tree1))
          (gen2 (tree->generator tree2)))
      (let loop ()
        (let ((leaf1 (gen1))
              (leaf2 (gen2)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))

(define tree->generator
  (lambda (tree)
    (let ((caller '*))
      (letrec
          ((generate-leaves
            (lambda ()
              (let loop ((tree tree))
                (cond ((null? tree) 'skip)
                      ((pair? tree)
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


(display
  (and
    (= (list-product1 '(4 2 1 3 5 1)) 120)
    (= (list-product1 '(2 8 0 1 9 6)) 0)
    (= (list-product2 '(4 2 6 3 1 5)) 720)
    (= (list-product2 '(7 9 2 0 6 4)) 0)
    (same-fringe1? '(1 (2 3)) '((1 2) 3))
    (not (same-fringe1? '(1 2 3) '(1 (3 2))))))
(newline)


