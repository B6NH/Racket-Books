;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ProDat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 23.7 Project: Database

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)


(define school-schema
  `(,(make-spec "Name" string?)
    ,(make-spec "Age" integer?)
    ,(make-spec "Present" boolean?)))

(define school-content
  '(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define presence-schema
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Description" string?)))

(define presence-content
  `((#true "presence")
    (#false "absence")))

(define school-db
  (make-db school-schema school-content))
(define presence-db
  (make-db presence-schema presence-content))
;; ------------------------------------------------------
(define bad-schema
  `(,(make-spec "Present" integer?)
    ,(make-spec "Description" integer?)))
(define bad-schema2
  `(,(make-spec "Name" string?)
    ,(make-spec "Age" integer?)
    ,(make-spec "Present" boolean?)
    ,(make-spec "Valid" boolean?)))
(define bad-db
  (make-db bad-schema school-content))
(define bad-db2
  (make-db bad-schema2 school-content))

;; Integrity Checking The use of databases

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
 
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check bad-db) #false)
(check-expect (integrity-check bad-db2) #false)
 
(define (integrity-check db)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define width   (length schema))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap (lambda (s c) [(spec-predicate s) c])
                         schema
                         row))))
    (andmap row-integrity-check content)))

;; Exercise 404. Design the function andmap2.
(check-expect
 (andmap2 <'(1 2 3)'(4 5 6))
 #true)
(check-expect
 (andmap2 <'(1 2 9)'(4 5 6))
 #false)
(check-expect
 (andmap2 <'(1 2 3)'(4 5 6 7))
 #false)


(define(andmap2 fun list1 list2)
  (cond
    ((and(empty? list1)(empty? list2))#true)
    ((or(empty? list1)(empty? list2))#false)
    (else(and(fun(first list1)(first list2))
             (andmap2 fun(rest list1)(rest list2))))))
  
;; A natural way to articulate the example as a test reuses figure 138: 

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
(define projected-schema
  `(,(make-spec "Name" string?) ,(make-spec "Present" boolean?)))
(define projected-db
  (make-db projected-schema projected-content))


(check-expect
  (db-content (project school-db '("Name" "Present")))
  projected-content)

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define (keep? c)
            (member? (spec-label c) labels))
          (define mask(map keep? schema))
          (define (row-project row)
            (foldr(lambda(x y r)
                    (if y(cons x r)r)) '() row mask)))
    (make-db (filter keep? schema)
             (map row-project content))))


;; Exercise 408. Design the function select.

(check-expect
 (db-content
  (select school-db
          '("Name" "Age")
          (lambda(row)
            (local((define name(first row)))
              (and(string? name)
                  (string=? name "Carol"))))))
 '(("Carol" 30)))
  

(define(select db labels predicate)
  (local((define content(db-content db))
         (define new-content(filter predicate content))
         (define new-db(make-db(db-schema db)new-content)))
    (project new-db labels)))
  


;; Exercise 409. Design reorder.

(define reordered-school-schema
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Name" string?)
    ,(make-spec "Age" integer?)))
(define reordered-school-content
  '((#true "Alice" 35)
    (#false "Bob" 25)
    (#true "Carol" 30)
    (#false "Dave" 32)))


(check-expect
 (db-content(reorder school-db '("Present" "Name" "Age")))
 reordered-school-content)

(check-expect
 (db-content(reorder school-db '("Name" "Age" "Present")))
 school-content)

(check-expect
 (db-content(reorder school-db '("Age" "Age")))
 (list
  (list 35 35 "Alice" #true)
  (list 25 25 "Bob" #false)
  (list 30 30 "Carol" #true)
  (list 32 32 "Dave" #false)))

(check-expect
 (length(db-schema(reorder school-db '("Age" "Age"))))
 4)

(check-expect
 (length(db-schema(reorder school-db '("Age" "Age" "Name" "Name"))))
 5)

(check-expect
 (length(db-schema(reorder school-db '("Present" "Name" "Age"))))
 3)

(check-expect
 (length(db-schema(reorder school-db '("Present" "Name"))))
 3)

(check-expect
 (length(db-schema(reorder school-db '("Present"))))
 3)

(check-expect
 (db-content(reorder school-db '("Present" "Name")))
 reordered-school-content)

(check-expect
 (db-content(reorder school-db '("Present")))
 reordered-school-content)

(check-expect
 (db-content(reorder school-db '()))
 school-content)

(check-error
 (reorder school-db '("Present" "NonExistentLabel")))

(define(absent-cells row newrow)
  (foldr(lambda(x y)(if(member? x newrow)y(cons x y)))'()row))

(check-expect
 (absent-cells'(1 2 3 4 5 6 7 22)'(4 5 6 7))
 '(1 2 3 22))

;; add absent cells at the end of newrow
(define(add-absent-cells-at-the-end row newrow)
  (add-at-the-end newrow(absent-cells row newrow)))

(check-expect
 (add-absent-cells-at-the-end'(1 2 3 4 5 6 7)'(4 5 6 7))
 '(4 5 6 7 1 2 3))

(define(add-at-the-end row end)
  (foldr(lambda(x y)(cons x y))end row))

(check-expect
 (add-at-the-end'(5 4 6)'(6 5 4))
 '(5 4 6 6 5 4))

(define(reorder db labels)
  (local
    ((define content(db-content db))
     (define schema(db-schema db))
     (define (reorder-row row)
       (local((define newrow (map (lambda(label)(cell-for-label label row schema)) labels)))
         (add-absent-cells-at-the-end row newrow)))
     (define (reorder-schema row)
       (local((define newrow (map (lambda(label)(spec-for-label label row)) labels)))
         (add-absent-cells-at-the-end row newrow))))
     (make-db (reorder-schema schema)
              (map reorder-row content))))

;; String List-of-Any List-of-Spec -> Any
(define(cell-for-label label row schema)
  (cond
    ((or(empty? row)(empty? schema))(error "WRONG"))
    (else
     (if(string=?(spec-label(first schema))
                 label)
        (first row)
        (cell-for-label label(rest row)(rest schema))))))

(check-error(cell-for-label "NonExistentLabel"(first school-content)school-schema))
(check-expect
 (cell-for-label "Age"(first school-content)school-schema)
 35)

;; String List-of-Spec -> Spec
(define(spec-for-label label schema)
  (local
    ((define result
       (memf(lambda(x)(string=? label(spec-label x)))schema)))
    (if(boolean? result)
       (error "WRONG")
       (first result))))

(check-error
 (spec-for-label "NonExistentLabel" school-schem))
(check-expect
 (spec?(spec-for-label "Present" school-schema))
 #true)

        
;; Exercise 410. Design the function db-union

(define other-content
  '(("Anette" 35 #false)
    ("Bob"   25 #false)
    ("Eve" 33 #true)
    ("Hanna"  37 #true)))

(define union-result-db
  (make-db school-schema
           '(("Alice" 35 #true)
             ("Bob"   25 #false)
             ("Carol" 30 #true)
             ("Dave"  32 #false)
             ("Anette" 35 #false)
             ("Eve" 33 #true)
             ("Hanna"  37 #true))))
  
(define other-db
  (make-db school-schema other-content))

(check-expect
 (db-content(db-union school-db other-db))
 (db-content union-result-db))

(check-expect
 (first-without-second '(1 2 3 4 5 6 7) '(2 3 5 6))
 '(1 4 7))

(define(first-without-second content1 content2)
  (foldr(lambda(x y)(if(member? x content2)y(cons x y)))'()content1))

(define(db-union db1 db2)
  (local
    ((define content1(db-content db1))
     (define content2(db-content db2))
     (define(union-content dbcontent1 dbcontent2)
       (append dbcontent1(first-without-second dbcontent2 dbcontent1))))
    (make-db(db-schema db1)
            (union-content content1 content2))))


;; Exercise 411. Design join

(define joined-db
  (make-db`(,(make-spec "Name" string?)
            ,(make-spec "Age" integer?)
            ,(make-spec "Description" string?))
          '(("Alice" 35 "presence")
            ("Bob"   25 "absence")
            ("Carol" 30 "presence")
            ("Dave"  32 "absence"))))

(define extended-presence-db
  (make-db
   `(,(make-spec "Present" boolean?)
     ,(make-spec "Description" string?)
     ,(make-spec "NewField" string?)
     ,(make-spec "LastField" number?))
   `((#true "presence" "abc" 123)
     (#false "absence" "def" 456))))


(define extended-presence-with-multiple-db
  (make-db
   `(,(make-spec "Present" boolean?)
     ,(make-spec "Description" string?)
     ,(make-spec "NewField" string?)
     ,(make-spec "LastField" number?))
   `((#true "presence" "abc" 123)
     (#true "here" "ggg" 777)
     (#false "absence" "def" 456)
     (#false "there" "ger" 111))))


(check-expect
 (db-content(join school-db presence-db))
 (db-content joined-db))

(check-expect
 (length(db-schema(join school-db presence-db)))
 (+(sub1(length(db-schema school-db)))
   (sub1(length(db-schema presence-db)))))

(check-expect
 (length(db-schema(join school-db extended-presence-db)))
 (+(sub1(length(db-schema school-db)))
   (sub1(length(db-schema extended-presence-db)))))

(check-expect
 (db-content(join school-db extended-presence-db))
 (db-content
  (make-db`(,(make-spec "Name" string?)
            ,(make-spec "Age" integer?)
            ,(make-spec "Description" string?)
            ,(make-spec "NewField" string?)
            ,(make-spec "LastField" number?))
          '(("Alice" 35 "presence" "abc" 123)
            ("Bob"   25 "absence" "def" 456)
            ("Carol" 30 "presence" "abc" 123)
            ("Dave"  32 "absence" "def" 456)))))


;; without last element from first list and
;; first element from second list
;; '(1 2 3) '(4 5 6) -> '(1 2 5 6)
(define(join-lists lst1 lst2)
  (cond
    ((empty?(rest lst1))(rest lst2))
    (else(cons(first lst1)
              (join-lists(rest lst1)lst2)))))


;; last item
(define(last lst)
  (cond
    ((empty?(rest lst))(first lst))
    (else(last(rest lst)))))

;; returns list of all corresponding rows
(define(all-rows-for-in row content)
  (local((define last-element(last row)))
    (foldr(lambda(x y)(if(equal? last-element(first x))(cons x y)y))'()content)))


(define(create-one-row-with-many row rows)
  (foldr(lambda(x y)(cons(join-lists row x)y))'()rows))

(check-expect
 (create-one-row-with-many
  '("Alice" 35 #true)
  '((#true "presence" "abc" 123)
    (#true "here" "ggg" 777)))
 '(("Alice" 35 "presence" "abc" 123)
   ("Alice" 35 "here" "ggg" 777)))


(define(join db1 db2)
  (local
    ((define content1(db-content db1))
     (define content2(db-content db2))
     (define(join-content content1 content2)
       (foldr
        (lambda(row y)
          (append
           (create-one-row-with-many row(all-rows-for-in row content2))
           y))
        '()content1)))
    (make-db
     (join-lists(db-schema db1)(db-schema db2))
     (join-content content1 content2))))


(check-expect
 (db-content(join school-db extended-presence-with-multiple-db))
 (db-content
  (make-db`(,(make-spec "Name" string?)
            ,(make-spec "Age" integer?)
            ,(make-spec "Description" string?)
            ,(make-spec "NewField" string?)
            ,(make-spec "LastField" number?))
          '(("Alice" 35 "presence" "abc" 123)
            ("Alice" 35 "here" "ggg" 777)
            ("Bob"   25 "absence" "def" 456)
            ("Bob"   25 "there" "ger" 111)
            ("Carol" 30 "presence" "abc" 123)
            ("Carol" 30 "here" "ggg" 777)
            ("Dave"  32 "absence" "def" 456)
            ("Dave"  32 "there" "ger" 111)))))


