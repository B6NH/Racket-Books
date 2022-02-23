
; Structures, Alists, Classes

; -------------------------------------------------------------------
; Structures & Alists
; -------------------------------------------------------------------

(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l)
          #f
          (if (eqv? (car l) o)
              i
              (loop (+ i 1) (cdr l)))))))

; Map function 'f' that returns list
(define append-map
  (lambda (f s)
    (let loop ((s s))
      (if (null? s)
          '()
          (append (f (car s)) (loop (cdr s)))))))

(define-macro defstruct
  (lambda (s . ff)
    (let ((s-s (symbol->string s)) (n (length ff)))
      (let* ((n+1 (+ n 1))
             (vv (make-vector n+1)))
        (let loop ((i 1) (ff ff))
          (if (<= i n)
            (let ((f (car ff)))
              (vector-set! vv i
                (if (pair? f) (cadr f) '(if #f #f)))
              (loop (+ i 1) (cdr ff)))))
        (let ((ff (map (lambda (f) (if (pair? f) (car f) f))
                       ff)))
          `(begin
             (define ,(string->symbol
                       (string-append "make-" s-s))
               (lambda fvfv
                 (let ((st (make-vector ,n+1)) (ff ',ff))
                   (vector-set! st 0 ',s)
                   ,@(let loop ((i 1) (r '()))
                       (if (>= i n+1) r
                           (loop (+ i 1)
                                 (cons `(vector-set! st ,i
                                          ,(vector-ref vv i))
                                       r))))
                   (let loop ((fvfv fvfv))
                     (if (not (null? fvfv))
                         (begin
                           (vector-set! st
                               (+ (list-position (car fvfv) ff)
                                  1)
                             (cadr fvfv))
                           (loop (cddr fvfv)))))
                   st)))
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string
                                     (list-ref ff (- i 1)))))
                             (cons
                              `(define ,(string->symbol
                                         (string-append
                                          s-s "." f))
                                 (lambda (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol
                                          (string-append
                                           "set!" s-s "." f))
                                  (lambda (x v)
                                    (vector-set! x ,i v)))
                               procs))))))
             (define ,(string->symbol (string-append s-s "?"))
               (lambda (x)
                 (and (vector? x)
                      (eqv? (vector-ref x 0) ',s))))))))))

; -------------------------------------------------------------------

; Structure definition
(defstruct tree height girth age leaf-shape leaf-color)

; Default values
(defstruct fruit name price (size 'medium) (color 'green))

(defstruct table (equ eqv?) (alist '()))

; -------------------------------------------------------------------

; Return key-value pair
(define lassoc
  (lambda (k al equ?)
    (let loop ((al al))
      (if (null? al)
          #f
          (let ((c (car al)))
            (if (equ? (car c) k)
                c
                (loop (cdr al))))))))

; Get table value
(define table-get
  (lambda (tbl k . d)
    (let ((c (lassoc k (table.alist tbl) (table.equ tbl))))
      (cond

        ; Return value from pair
        (c (cdr c))

        ; Return default value
        ((pair? d) (car d))))))

(define table-put!
  (lambda (tbl k v)

    ; List of key-value pairs
    (let ((al (table.alist tbl)))

      ; Find key-value pair
      (let ((c (lassoc k al (table.equ tbl))))

        ; If pair exists set its cdr
        ; Otherwise add new pair to table
        (if c
            (set-cdr! c v)
            (set!table.alist tbl (cons (cons k v) al)))))))

(define table-for-each
  (lambda (tbl p)
    (for-each
      (lambda (c) (p (car c) (cdr c)))
      (table.alist tbl))))

(define show-key-val
  (lambda (k v)
    (display "Key - Val: ")
    (display k)
    (display " ")
    (display v)
    (newline)))

; -------------------------------------------------------------------

; Creating structure instance
(define coconut
  (make-tree
    'height 50
    'leaf-shape 'frond
    'age 2))

(define frt1
  (make-fruit 'name 'apple))

(define frt2
  (make-fruit 'color 'red 'price 4))

; Create empty table
(define table1 (make-table))

; -------------------------------------------------------------------

(begin

  (display "----- Structures & Alists -----\n")

  (display
    (and
      (= (tree.height coconut) 50)
      (= (tree.age coconut) 2)
      (eqv? (tree.leaf-shape coconut) 'frond)))
  (newline)

  ; Update fields
  (set!tree.height coconut 60)
  (set!tree.leaf-color coconut 'green)

  (display
    (and
      (= (tree.height coconut) 60)
      (eqv? (tree.leaf-color coconut) 'green)))
  (newline)

  ; Void values
  (display (tree.girth coconut)) (newline)
  (display (fruit.price frt1)) (newline)
  (display (fruit.name frt2)) (newline)

  (display
    (and
      (eqv? (fruit.name frt1) 'apple)
      (eqv? (fruit.size frt1) 'medium)
      (eqv? (fruit.color frt1) 'green)
      (eqv? (fruit.color frt2) 'red)
      (eqv? (fruit.size frt2) 'medium)
      (eqv? (fruit.price frt2) 4)))
  (newline)

  ; Put data into table
  (table-put! table1 'color 'red)
  (table-put! table1 'height 10)
  (table-put! table1 'width 5)

  ; Call function for each value in table
  (table-for-each table1 show-key-val)

  (display
    (and
      (equal? (lassoc 'height (table.alist table1) eqv?) '(height . 10))
      (equal? (lassoc 'width (table.alist table1) eqv?) '(width . 5))
      (equal? (lassoc 'color (table.alist table1) eqv?) '(color . red))
      (equal? (table-get table1 'color) 'red)
      (equal? (table-get table1 'size 'default-size) 'default-size)))
  (newline))

; -------------------------------------------------------------------
; Classes
; -------------------------------------------------------------------

(defstruct standard-class
  slots superclass method-names method-vector)

; Return class of instance (vector)
(define class-of1
  (lambda (instance)
    (vector-ref instance 0)))

; Return class of any Scheme object
(define class-of2
  (lambda (x)
    (if (vector? x)
        (let ((n (vector-length x)))
          (if (>= n 1)
              (let ((c (vector-ref x 0)))
                (if (standard-class? c) c #t))
              #t))
        #t)))

; Get instance slot value
(define slot-value
  (lambda (instance slot)
    (let* ((class (class-of2 instance))
           (slot-index (list-position slot (standard-class.slots class))))
      (vector-ref instance (+ slot-index 1)))))

; Set instance slot value
(define set!slot-value
  (lambda (instance slot new-val)
    (let* ((class (class-of2 instance))
           (slot-index (list-position slot (standard-class.slots class))))
      (vector-set! instance (+ slot-index 1) new-val))))

(define slot-value-m
  (lambda (instance slot)
    (let* ((class (class-of1 instance))
           (slot-index (list-position slot (vector-ref class 1))))
      (vector-ref instance (+ slot-index 1)))))

(define set!slot-value-m
  (lambda (instance slot new-val)
    (let* ((class (class-of1 instance))
           (slot-index (list-position slot (vector-ref class 1))))
      (vector-set! instance (+ slot-index 1) new-val))))

; Remove duplicates from list
(define delete-duplicates
  (lambda (s)
    (if (null? s)
        s
        (let ((a (car s)) (d (cdr s)))
          (if (memv a d)
              (delete-duplicates d)
              (cons a (delete-duplicates d)))))))

; Create subclass
(define create-class-proc
  (lambda (superclass slots method-names method-vector)
    (make-standard-class

     'superclass
       superclass

     'slots

       ; Set superclass slots
       (let ((superclass-slots
               (if (not (eqv? superclass #t))
                   (standard-class.slots superclass)
                   '())))

         (if (null? superclass-slots)

             ; Keep original slots
             slots

             ; Append slots and remove duplicates
             (delete-duplicates (append slots superclass-slots))))

     'method-names
       method-names

     'method-vector
       method-vector)))

(define create-class-object-proc
  (lambda (direct-superclasses slots method-names method-vector)

    ; List of superclasses without duplicates
    (let ((class-precedence-list
            (delete-duplicates
              (append
                direct-superclasses
                (append-map (lambda (c) (vector-ref c 2)) direct-superclasses)))))
      (send-m
        'make-instance-object
          standard-class-object
        'class-precedence-list
          class-precedence-list
        'slots
          (delete-duplicates
            (append
              slots
              (append-map
                (lambda (c) (vector-ref c 1))
                class-precedence-list)))
        'method-names
          method-names
        'method-vector
          method-vector))))

; Macro to create classes with single inheritance
(define-macro create-class
  (lambda (superclass slots . methods)
    `(create-class-proc
      ,superclass
      (list ,@(map (lambda (slot) `',slot) slots))
      (list ,@(map (lambda (method) `',(car method)) methods))
      (vector ,@(map (lambda (method) `,(cadr method)) methods)))))

; Macro to create classes with multiple inheritance
(define-macro create-class-object
  (lambda (direct-superclasses slots . methods)
    `(create-class-object-proc
      (list ,@(map (lambda (su) `,su) direct-superclasses))
      (list ,@(map (lambda (slot) `',slot) slots))
      (list ,@(map (lambda (method) `',(car method)) methods))
      (vector ,@(map (lambda (method) `,(cadr method)) methods)))))

; Make class instance (vector)
; Its first element is class object
; Remaining elements are slot values
; For example: #(#(standard-class (frame parts size) #t () #()) cromoly alivio 18.5)
(define make-instance
  (lambda (class . slot-value-twosomes)

    ; List of slots
    (let* ((slotlist (standard-class.slots class))

           ; Number of slots
           (n (length slotlist))

           ; Object vector (slots + class)
           (instance (make-vector (+ n 1))))

      ; Set object class
      (vector-set! instance 0 class)

      ; Loop through slot-value pairs
      (let loop ((slot-value-twosomes slot-value-twosomes))
        (if (null? slot-value-twosomes)

            ; Return new object
            instance

            ; Find slot index
            (let ((k (list-position (car slot-value-twosomes) slotlist)))

              ; Set slot value
              (vector-set! instance (+ k 1) (cadr slot-value-twosomes))

              ; Skip 2 processed elements
              (loop (cddr slot-value-twosomes))))))))

(define make-instance-object
  (lambda (class . slot-value-twosomes)
    (let* ((slotlist (vector-ref class 1))
           (n (length slotlist))
           (instance (make-vector (+ n 1))))
      (vector-set! instance 0 class)
      (let loop ((slot-value-twosomes slot-value-twosomes))
        (if (null? slot-value-twosomes)
            instance
            (let ((k (list-position (car slot-value-twosomes) slotlist)))
              (vector-set! instance (+ k 1) (cadr slot-value-twosomes))
              (loop (cddr slot-value-twosomes))))))))

; Call method on object / Send message to object (single inheritance)
(define send
  (lambda (method instance . args)
    (let ((proc

           ; Look for method
           (let loop ((class (class-of2 instance)))
             (if (eqv? class #t)

                 ; Method not found
                 (error 'send)

                 ; Look for method in current class
                 (let ((k (list-position method (standard-class.method-names class))))
                   (if k

                       ; Method found
                       (vector-ref (standard-class.method-vector class) k)

                       ; Look for method in superclass
                       (loop (standard-class.superclass class))))))))

      ; Apply found method
      (apply proc instance args))))

; Send with multiple inheritance
(define send-m
  (lambda (method-name instance . args)
    (let ((proc (let ((class (class-of1 instance)))
             (if (eqv? class #t)

                 ; Method not found
                 (error 'send-ma)

                 ; Start loop with current class and list of superclasses
                 (let loop ((class class) (superclasses (vector-ref class 2)))

                   (let ((k (list-position method-name (vector-ref class 3))))
                     (cond

                       ; Method found in current class
                       (k (vector-ref (vector-ref class 4) k))

                       ; No more superclasses
                       ((null? superclasses) (error 'send-mo))

                       ; Check next superclass
                       (else (loop (car superclasses) (cdr superclasses))))))))))

      (apply proc instance args))))

; Create simple standard class
(define simple-bike-class
  (make-standard-class
    'superclass #t
    'slots '(frame parts size)
    'method-names '()
    'method-vector #()))

; Create class using 'create-class' macro
(define bike-class
  (create-class

   ; Superclass
   #t

   ; Slots
   (frame size parts chain tires)

   ; Method(s)
   ; Parameter 'me' is current object
   (check-fit (lambda (me inseam)
                (let ((bike-size (slot-value me 'size))
                      (ideal-size (* inseam 3/5)))
                  (let ((diff (- bike-size ideal-size)))
                    (cond
                      ((<= -1 diff 1) 'perfect-fit)
                      ((<= -2 diff 2) 'fits-well)
                      ((< diff -2) 'too-small)
                      ((> diff 2) 'too-big))))))))

; Create subclass of bike-class
(define mtn-bike-class
  (create-class

    ; Superclass
    bike-class

    ; New slot
    (suspension)

    ; Overridden method
    (check-fit (lambda (me inseam)
                 (let ((bike-size (slot-value me 'size))
                       (ideal-size (- (* inseam 3/5) 2)))
                   (let ((diff (- bike-size ideal-size)))
                     (cond
                       ((<= -2 diff 2) 'perfect-fit)
                       ((<= -4 diff 4) 'fits-well)
                       ((< diff -4) 'too-small)
                       ((> diff 4) 'too-big))))))))

; Class as object
(define standard-class-object
  (vector

    ; Placeholder
    'value-of-standard-class

    ; Slots
    (list
      'slots
      'class-precedence-list
      'method-names
      'method-vector)

    ; Class precedence list
    '()

    ; Method names
    '(make-instance-object)

    ; Method vector
    (vector make-instance-object)))

; Class is instance of itself
(vector-set! standard-class-object 0 standard-class-object)

; Create car class based on standard-class-object
(define car-class
  (create-class-object
    ()
    (name color)
    (show-name
      (lambda (me)
        (display (slot-value-m me 'name))))
    (show-color
      (lambda (me)
        (display (slot-value-m me 'color))))))

(define plane-class
  (create-class-object
    ()
    (size)
    (fly
      (lambda (me)
        (display "Flying")))
    (land
      (lambda (me)
        (display "Landing")))))

(define boat-class
  (create-class-object
    ()
    (mass)
    (sail
      (lambda (me)
        (display "Sailing")))))

; Class inheriting from: 'car-class', 'plane-class', 'boat-class'
(define sub-class
  (create-class-object
    (car-class plane-class boat-class)
    (sub-slot)
    (do-everything
      (lambda (me)
        (display "Doing Everything")))))

; -------------------------------------------------------------------

(define my-bike1
  (make-instance
    simple-bike-class
      'frame 'cromoly
      'size '18.5
      'parts 'alivio))

(define my-bike2
  (make-instance
    bike-class
      'frame 'titanium
      'size 21
      'parts 'ultegra
      'chain 'sachs
      'tires 'continental))

(define my-bike3
  (make-instance
    mtn-bike-class
      'frame 'hiten
      'size 25
      'parts 'acera
      'chain 'shimano
      'tires 'michelin
      'suspension 'ceriani))

(define my-car
  (make-instance-object
    car-class
      'name 'volvo
      'color 'red))

(define sub-object
  (make-instance-object
    sub-class
    'name "Viper"
    'color 'gold
    'size 'large
    'mass 500
    'sub-slot "SubValue"))

; -------------------------------------------------------------------

(begin

  (display "----- Classes -----\n")

  (display

    (and

      (eqv? (send 'check-fit my-bike2 25) 'too-big)
      (eqv? (send 'check-fit my-bike2 32) 'fits-well)
      (eqv? (send 'check-fit my-bike2 36) 'perfect-fit)
      (eqv? (send 'check-fit my-bike2 50) 'too-small)

      (eqv? (send 'check-fit my-bike3 32) 'too-big)
      (eqv? (send 'check-fit my-bike3 40) 'fits-well)
      (eqv? (send 'check-fit my-bike3 45) 'perfect-fit)
      (eqv? (send 'check-fit my-bike3 55) 'too-small)

      (eqv? (slot-value-m my-car 'name) 'volvo)
      (eqv? (slot-value-m my-car 'color) 'red)

      (equal? (slot-value-m sub-object 'name) "Viper")
      (equal? (slot-value-m sub-object 'sub-slot) "SubValue")
      (eqv? (slot-value-m sub-object 'color) 'gold)
      (eqv? (slot-value-m sub-object 'size) 'large)
      (= (slot-value-m sub-object 'mass) 500)))

  (newline)

  (send-m 'show-name my-car) (newline)
  (send-m 'show-color my-car) (newline)

  (display "Subobject:\n")

  ; Car
  (send-m 'show-name sub-object) (newline)
  (send-m 'show-color sub-object) (newline)

  ; Plane
  (send-m 'fly sub-object) (newline)
  (send-m 'land sub-object) (newline)

  ; Boat
  (send-m 'sail sub-object) (newline)

  ; Subobject
  (send-m 'do-everything sub-object) (newline))

