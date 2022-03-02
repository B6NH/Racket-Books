
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
      (if (null? s)
          1
          (* (car s) (recur (cdr s)))))))

; Use continuation to quit function when number 0 is encountered
(define list-product2
  (lambda (s)
    (call/cc
      (lambda (exit)
        (let recur ((s s))
          (cond
            ((null? s) 1)
            ((= (car s) 0) (exit 0))
            (else (* (car s) (recur (cdr s))))))))))

; Flatten nested lists
(define flatten
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((pair? (car tree))
        (append (flatten (car tree))
                (flatten (cdr tree))))
      (else
        (cons (car tree) (flatten (cdr tree)))))))

; Flatten lists and check all elements
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

; Compare all leaves using generators
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

(define same-fringe3?
  (lambda (tree1 tree2)
    (letrec

      ; Tree coroutines
      ((tree-cor-1
         (make-leaf-gen-cor
           tree1
           (lambda (v) (matcher-cor v))))
       (tree-cor-2
         (make-leaf-gen-cor
           tree2
           (lambda (v) (matcher-cor v))))

       ; Matcher coroutine depends on tree coroutines
       (matcher-cor
         (make-matcher-cor
           (lambda (v) (tree-cor-1 v))
           (lambda (v) (tree-cor-2 v)))))

      (matcher-cor 'start-the-ball-rolling))))

(define tree->generator
  (lambda (tree)

    ; Continuation will be stored in caller
    (let ((caller '*))

      (letrec

          ; Internal procedure
          ((generate-leaves
            (lambda ()

              ; Go through tree
              (let loop ((tree tree))
                (cond

                  ; Empty tree
                  ((null? tree) 'skip)

                  ; Continue loop
                  ((pair? tree)

                   ; Process first element
                   (loop (car tree))

                   ; Process rest of tree
                   (loop (cdr tree)))

                  ; Leaf
                  (else
                    (call/cc
                      (lambda (rest-of-tree)

                        ; Capture loop continuation
                        (set! generate-leaves
                          (lambda ()
                            (rest-of-tree 'resume)))

                        ; Return leaf value to caller
                        (caller tree))))))

              ; Return empty list to caller
              (caller '()))))

            ; Function returned
            (lambda ()
              (call/cc
                (lambda (k)

                  ; Save current continuation in caller
                  (set! caller k)

                  ; Call function to generate leaf
                  (generate-leaves))))))))

; Create coroutine procedure
(define-macro coroutine
  (lambda (x . body)
    `(letrec ((+local-control-state
               (lambda (,x) ,@body))

              ; Resume another coroutine with transfer value
              (resume
               (lambda (c v)
                 (call/cc
                   (lambda (k)
                     (set! +local-control-state k)
                     (c v))))))

       ; Return unary function
       (lambda (v)
         (+local-control-state v)))))

(define make-matcher-cor
  (lambda (tree-cor-1 tree-cor-2)
    (coroutine
      dummy-init-arg
      (let loop ()
        (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
              (leaf2 (resume tree-cor-2 'get-a-leaf)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))

(define make-leaf-gen-cor
  (lambda (tree matcher-cor)
    (coroutine
      dummy-init-arg
      (let loop ((tree tree))
        (cond
          ((null? tree) 'skip)
          ((pair? tree)
           (loop (car tree))
           (loop (cdr tree)))
          (else (resume matcher-cor tree))))
      (resume matcher-cor '()))))

(define make-location-cor
  (lambda (other-location-cor manager-cor)
    (coroutine v
      (let ((num-umbrellas 1))
        (let loop ((umbrella? (car v))
                   (walks-so-far (cadr v)))
          (when umbrella?
            (set! num-umbrellas (+ num-umbrellas 1)))
          (cond
            ((>= walks-so-far *max-num-walks*)
             (resume manager-cor walks-so-far))
            ((< (random) *rain-prob*)
             (cond
               ((> num-umbrellas 0)
                (set! num-umbrellas (- num-umbrellas 1))
                (apply loop
                  (resume other-location-cor
                  (list #t (+ walks-so-far 1)))))
               (else (apply loop (resume manager-cor walks-so-far)))))
            (else
              (apply loop (resume other-location-cor (list #f (+ walks-so-far 1)))))))))))

(define make-manager-cor
  (lambda (home-cor)
    (coroutine dummy-init-arg
      (resume home-cor (list #f 0)))))

(define *rain-prob* 0.4)
(define *max-num-walks* (* 365 2 5)) ; 2 walks per day for 5 years

(begin
  (display
    (and

      (equal? (flatten '((1 2) (3 (4 5)) (6) ((7)))) '(1 2 3 4 5 6 7))

      (= (list-product1 '(4 2 1 3 5 1)) 120)
      (= (list-product1 '(2 8 0 1 9 6)) 0)

      (= (list-product2 '(4 2 6 3 1 5)) 720)
      (= (list-product2 '(7 9 2 0 6 4)) 0)

      (same-fringe1? '(1 (2 3)) '((1 2) 3))
      (not (same-fringe1? '(1 2 3) '(1 (3 2))))

      (same-fringe2? '(1 (2 (3))) '((1 (2)) 3))
      (not (same-fringe2? '(6 (7 (4) 1) 2) '((4 5) 3 6 (1))))

      (same-fringe3? '(((8 1) 5) ((4) 2)) '((8 (1 5) 4 2)))
      (not (same-fringe3? '(7 1 4 3 1) '((1 (4 3)) (2 9))))))

  (newline))


