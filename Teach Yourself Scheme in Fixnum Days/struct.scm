
; Structures, Alists, Classes

; -------------------------------------------------------------------

(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l)
          #f
          (if (eqv? (car l) o)
              i
              (loop (+ i 1) (cdr l)))))))

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

(defstruct standard-class
  slots superclass method-names method-vector)

; Return class of instance (vector)
(define class-of1
  (lambda (instance)
    (vector-ref instance 0)))

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
    (let* ((class (class-of instance))
           (slot-index (list-position slot (standard-class.slots class))))
      (vector-ref instance (+ slot-index 1)))))

; Set instance slot value
(define set!slot-value
  (lambda (instance slot new-val)
    (let* ((class (class-of instance))
           (slot-index (list-position slot (standard-class.slots class))))
      (vector-set! instance (+ slot-index 1) new-val))))

(define simple-bike-class
  (make-standard-class
   'superclass #t
   'slots '(frame parts size)
   'method-names '()
   'method-vector #()))

(define delete-duplicates
  (lambda (s)
    (if (null? s)
        s
        (let ((a (car s)) (d (cdr s)))
          (if (memv a d)
              (delete-duplicates d)
              (cons a (delete-duplicates d)))))))

(define create-class-proc
  (lambda (superclass slots method-names method-vector)
    (make-standard-class
     'superclass
       superclass
     'slots
       (let ((superclass-slots
               (if (not (eqv? superclass #t))
                   (standard-class.slots superclass)
                   '())))
         (if (null? superclass-slots)
             slots
             (delete-duplicates (append slots superclass-slots))))
     'method-names
       method-names
     'method-vector
       method-vector)))

(define-macro create-class
  (lambda (superclass slots . methods)
    `(create-class-proc
      ,superclass
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

(define send #t)

(define my-bike
  (make-instance
    simple-bike-class
      'frame 'cromoly
      'size '18.5
      'parts 'alivio))

