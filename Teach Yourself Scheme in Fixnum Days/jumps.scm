(include "helper/macros.scm")
(include "helper/functions.scm")
(random-source-randomize! default-random-source)

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

; Multiply list of numbers (named let)
(define list-product1
  (lambda (init-val)
    (let recur ((arg init-val))
      (if (null? arg)
          1
          (* (car arg) (recur (cdr arg)))))))

; Named let as letrec
(define list-product11
  (lambda (init-val)
    (letrec ((recur (lambda (arg)
                      (if (null? arg)
                          1
                          (* (car arg) (recur (cdr arg)))))))
      (recur init-val))))

; Use continuation to quit function when number 0 is encountered
(define list-product2
  (lambda (s)
    (call/cc
      (lambda (exit)
        (let recur ((s s))
          (cond

            ; End of list
            ((null? s) 1)

            ; Return zero immediately
            ((= (car s) 0) (exit 0))

            ; Continue multiplication
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

        ; Two empty trees
        ((and (null? ftree1) (null? ftree2)) #t)

        ; One of trees is non-empty
        ((or (null? ftree1) (null? ftree2)) #f)

        ; Continue loop
        ((eqv? (car ftree1) (car ftree2))
         (loop (cdr ftree1) (cdr ftree2)))

        ; Elements differ
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

      ; Call matcher coroutine
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

              ; Resume another coroutine (c) with transfer value (v)
              (resume
               (lambda (c v)
                 (call/cc
                   (lambda (k)

                     ; Save continuation
                     (set! +local-control-state k)

                     ; Resume coroutine
                     (c v))))))

       ; Return unary function
       ; Local control state is initially entire coroutine computation
       (lambda (v)
         (+local-control-state v)))))

(define make-matcher-cor
  (lambda (tree-cor-1 tree-cor-2)
    (coroutine
      dummy-init-arg
      (let loop ()

        ; Get leaves from tree coroutines
        (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
              (leaf2 (resume tree-cor-2 'get-a-leaf)))

          ; Compare leaves
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

          ; Resume matcher coroutine with leaf argument
          (else (resume matcher-cor tree))))

      (resume matcher-cor '()))))

; Create location coroutine
; Walk between home and work until you get wet
; or if maximum number of walks has been reached
(define make-location-cor
  (lambda (other-location-cor manager-cor)
    (coroutine v

      ; Each location starts with one umbrella
      (let ((num-umbrellas 1))
        (let loop ((umbrella? (car v)) ; did umbrella arrive during last walk
                   (walks-so-far (cadr v))) ; number of walks

          ; Increase umbrella count
          (when umbrella?
            (set! num-umbrellas (+ num-umbrellas 1)))

          (cond

            ; Return result to manager coroutine
            ((>= walks-so-far *max-num-walks*)
             (resume manager-cor walks-so-far))

            ; Raining
            ((< (random-real) *rain-prob*)
             (cond

               ; Take umbrella
               ((> num-umbrellas 0)
                (set! num-umbrellas (- num-umbrellas 1))
                (apply loop
                  (resume other-location-cor (list #t (+ walks-so-far 1)))))

               ; No umbrellas
               ; Resume to manager coroutine
               (else (apply loop
                       (resume manager-cor walks-so-far)))))

            ; Not raining
            (else
              (apply loop
                (resume other-location-cor (list #f (+ walks-so-far 1)))))))))))

; Start at home and wait for response from one of the locations
(define make-manager-cor
  (lambda (home-cor)
    (coroutine dummy-init-arg
      (resume home-cor (list #f 0)))))

(define *rain-prob* 0.4)
(define *max-num-walks* (* 365 2 5)) ; 2 walks per day for 5 years
(define *num-trials* 5000)
(define wa "Wet after ")
(define da " days")

(define umbrella-trial
  (lambda (rain-prob)
    (lambda ()
      (when (number? rain-prob) (set! *rain-prob* rain-prob))

        ; Create coroutines
        (letrec ((home-cor (make-location-cor
                             (lambda (v) (office-cor v))
                              (lambda (v) (manager-cor v))))
                 (office-cor (make-location-cor
                               (lambda (v) (home-cor v))
                               (lambda (v) (manager-cor v))))
                 (manager-cor (make-manager-cor
                                (lambda (v) (home-cor v)))))

                ; Start trial
                (manager-cor 'start-the-ball-rolling)))))

(begin
  (display
    (and

      (equal? (flatten '((1 2) (3 (4 5)) (6) ((7)))) '(1 2 3 4 5 6 7))

      (= (list-product1 '(4 2 1 3 5 1)) 120)
      (= (list-product1 '(2 8 0 1 9 6)) 0)

      (= (list-product11 '(7 4 2 8 6 5)) 13440)
      (= (list-product11 '(9 2 0 8 7 1)) 0)

      (= (list-product2 '(4 2 6 3 1 5)) 720)
      (= (list-product2 '(7 9 2 0 6 4)) 0)

      (same-fringe1? '(1 (2 3)) '((1 2) 3))
      (not (same-fringe1? '(1 2 3) '(1 (3 2))))

      (same-fringe2? '(1 (2 (3))) '((1 (2)) 3))
      (not (same-fringe2? '(6 (7 (4) 1) 2) '((4 5) 3 6 (1))))

      (same-fringe3? '(((8 1) 5) ((4) 2)) '((8 (1 5) 4 2)))
      (not (same-fringe3? '(7 1 4 3 1) '((1 (4 3)) (2 9))))))

  (newline)
  (display
    (string-append
      wa
      (number->string ((umbrella-trial *rain-prob*)))
      da))

  (newline)

  (display
    (string-append
      wa
      (number->string (monte-carlo (umbrella-trial *rain-prob*)))
      da " (average)"))

  (newline))
