; Database

(require (planet dyoo/simply-scheme:2:2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions

;; Load program in racket interactive mode
;; (load "database.rkt")

;; Create new database
;; (new-db "albums" '(artist title year brian-likes?))

;; Insert data
;; (insert)

;; Show data
;; (list-db)

;; Show number of records in current database
;; (count-db)

;; Edit record at index
;; (edit-record 2)

;; Add 'category field with default value 'rock
;; (add-field 'category 'rock)

;; Add field 'matt-likes? with default value #f
;; (add-field 'matt-likes?)

;; Sort database
;; (sort-on 'year)

;; Select some records
;; (select-records '(2 4))

;; Save selected records in new file
;; (save-selection "selected-albums")

;; Save database (file name is the same as database name)
;; (save-db)

;; Load database
;; (load-db "albums")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The database ADT: a filename, list of fields and list of records

(define (make-db filename fields records)
  (vector filename fields records))

(define (db-filename db)
  (vector-ref db 0))

(define (db-set-filename! db filename)
  (vector-set! db 0 filename))

(define (db-fields db)
  (vector-ref db 1))

(define (db-set-fields! db fields)
  (vector-set! db 1 fields))

(define (db-records db)
  (vector-ref db 2))

(define (db-set-records! db records)
  (vector-set! db 2 records))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-state (vector #f))

(define selected-records '())

(define (no-db?)
  (not (vector-ref current-state 0)))

(define (current-db)
  (if (no-db?)
      (error "No current database!")
      (vector-ref current-state 0)))

(define (set-current-db! db)
  (vector-set! current-state 0 db))

(define (current-fields)
  (db-fields (current-db)))

(define (current-records)
  (db-records (current-db)))

(define (new-db filename fields)
  (clear-current-db!)
  (set-current-db! (make-db filename fields '()))
  'created)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insert)
  (let ((new-record (get-record)))
    (db-insert new-record (current-db)))
  (if (ask "Insert another? ")
      (insert)
      'inserted))

(define (db-insert record db)
  (db-set-records! db (cons record (db-records db))))

(define (set-current-records! records)
  (db-set-records! (current-db) records))

(define (get-record)
  (get-record-loop 0
    (blank-record)
    (current-fields)))

(define (get-record-loop field-index record fields)
  (if (null? fields)
      record
      (begin
        (display "Value for ")
        (display (car fields))
        (display "--> ")
        (vector-set! record field-index (read))
        (get-record-loop (add1 field-index) record (cdr fields)))))

(define (ask question)
  (display question)
  (let ((answer (read)))
    (cond
      ((equal? (first answer) 'y) #t)
      ((equal? (first answer) 'n) #f)
      (else (show "Please type Y or N.")
            (ask question)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-db)
  (length (db-records (current-db))))

;; Display one record
(define (show-record fields record)
  (show-record-helper fields record 0)
  (show ""))

(define (show-record-helper fields record index)
  (if (empty? fields)
      'done
      (begin
        (display (first fields))
        (display ": ")
        (show (vector-ref record index))
        (show-record-helper (cdr fields) record (add1 index)))))

;; Display list of records
(define (show-records fields records)
  (show-records-helper fields records 1))

(define (show-records-helper fields records index)
  (if (empty? records)
      'listed
      (begin
        (show (string-append "RECORD " (number->string index)))
        (show-record fields (car records))
        (show-records-helper fields (cdr records) (add1 index)))))

;; List current database records
(define (list-db)
  (show-records (current-fields) (current-records)))

;; Get record at index
(define (get-record-at index records)
  (get-record-at-helper index records 1))

(define (get-record-at-helper index records current-index)
  (if (= index current-index)
      (car records)
      (get-record-at-helper index (cdr records) (add1 current-index))))

;; Get record from current database records
(define (get-current-record-at index)
  (get-record-at index (current-records)))

(define (get-field-index field fields)
  (get-field-index-helper field fields 0))

(define (get-field-index-helper field fields index)
  (if (equal? field (car fields))
      index
      (get-field-index-helper field (cdr fields) (add1 index))))

(define (get-current-field-index field)
  (get-field-index field (current-fields)))

(define (show-record-with-fields record)
  (show-record (current-fields) record))

(define (current-db-name)
  (db-filename (current-db)))

(define (edit-record index)
  (let ((rec (get-current-record-at index)))
    (show-record-with-fields rec)
    (display "Edit which field? ")
    (let ((field (read)))
      (display "New value for ")
      (display field)
      (record-set! field rec (read))
      (show-record-with-fields rec))))

(define (save-db)
  (let ((port (open-output-file (current-db-name))))
    (write (current-db) port)
    (close-output-port port)
    'saved))

(define (load-db filename)
  (let ((port (open-input-file filename)))
    (clear-current-db!)
    (set-current-db! (read port))
    (close-input-port port)
    'loaded))

(define (clear-current-db!)
 (if (no-db?)
     'nothing
     (begin
       (if (ask "Save current database?")
           (save-db)
           'done)
       (clear-selected-records!)
       (set-current-db! #f)
       'cleared)))

(define (clear-selected-records!)
  (set! selected-records '()))

(define (get field-name record)
  (vector-ref record (get-current-field-index field-name)))

;; Create empty record
(define (blank-record)
  (make-vector (length (current-fields))))

;; Edit record field
(define (record-set! field-name record new-value)
  (vector-set! record (get-current-field-index field-name) new-value))

(define (copy-vector old new)
  (copy-vector-helper old new 0))

(define (copy-vector-helper old new index)
  (if (= index (vector-length old))
      new
      (begin
        (vector-set! new (add1 index) (vector-ref old index))
        (copy-vector-helper old new (add1 index)))))

(define (set-current-fields! fields)
  (db-set-fields! (current-db) fields))

(define (adjoin-field record new-value)
  (let ((vec (make-vector (add1 (vector-length record)))))
    (vector-set! vec 0 new-value)
    (copy-vector record vec)))

(define (adjoin-all-records records value)
  (if (empty? records)
      records
      (cons (adjoin-field (car records) value)
            (adjoin-all-records (cdr records) value))))

(define (selection-sort records predicate)
  (if (empty? records)
      records
      (let ((er (earliest-record records predicate)))
        (cons er (selection-sort (remove-once er records) predicate)))))

(define (earliest-record records predicate)
  (earliest-helper (car records) (cdr records) predicate))

(define (earliest-helper so-far rest predicate)
  (if (empty? rest)
      so-far
      (let ((fst (car rest)) (rst (cdr rest)))
        (if (predicate so-far fst)
            (earliest-helper so-far rst predicate)
            (earliest-helper fst rst predicate)))))

(define (remove-once record records)
  (if (empty? records)
       '()
      (let ((fst (car records))
            (rst (cdr records)))
        (if (equal? record fst)
             rst
             (cons fst (remove-once record rst))))))

(define (sort predicate)
  (set-current-records!
    (selection-sort (current-records) predicate))
  'sorted)

(define (sort-on-by field-name predicate)
  (sort (lambda (r1 r2)
          (predicate
            (get field-name r1) (get field-name r2))))
  'sorted)

(define (before-list? lst1 lst2)
  (cond
    ((empty? lst1) #t)
    ((empty? lst2) #f)
    (else
      (let ((f1 (car lst1)) (f2 (car lst2)))
        (cond
          ((generic-before? f1 f2) #t)
          ((generic-before? f2 f1) #f)
          (else (before-list? (cdr lst1) (cdr lst2))))))))

(define (before-lst-wrd lst wrd)
  (generic-before? (car lst) wrd))

(define (before-wrd-lst wrd lst)
  (generic-before? wrd (car lst)))

(define (generic-before? arg1 arg2)
  ((cond
    ((and (number? arg1) (number? arg2)) <)
    ((and (list? arg1) (list? arg2)) before-list?)
    ((and (list? arg1) (word? arg2)) before-lst-wrd)
    ((and (word? arg1) (list? arg2)) before-wrd-lst)
    (else before?))
   arg1 arg2))

(define (sort-on field-name)
  (sort-on-by field-name generic-before?))

(define (add-field field-name . initial-value)
  (set-current-fields!
    (cons field-name (current-fields)))
  (set-current-records!
    (adjoin-all-records
      (current-records)
      (if (empty? initial-value)
          #f
          (car initial-value))))
  'added)

(define (remove-duplicates lst)
  (if (empty? lst)
      lst
      (let ((fst (car lst)) (rst (cdr lst)))
        (let ((next (remove-duplicates rst)))
          (if (member fst rst)
              next
              (cons fst next))))))

;; Selected index should be in range from 1 to number of records
(define (check-selection-range-helper lst max)
  (or (empty? lst)
      (and (>= (car lst) 1)
           (<= (car lst) max)
           (check-selection-range-helper (cdr lst) max))))

;; Check if all selected records have valid indices
(define (check-selection-range lst)
  (check-selection-range-helper lst (count-db)))

;; Select records to save
(define (select-records indices)
  (let ((lst (remove-duplicates indices)))
    (if (check-selection-range lst)
        (set! selected-records lst)
        (error "Record index out of range"))))

;; Create list with selected records
(define (make-selection-list)
  (make-selection-list-helper (current-records) 1))

(define (make-selection-list-helper records record-index)
  (if (empty? records)
      '()
      (let ((next (make-selection-list-helper (cdr records) (add1 record-index))))
        (if (member record-index selected-records)
            (cons (car records) next)
            next))))

;; Save database copy with selected records in new file
(define (save-selection filename)
  (let ((port (open-output-file filename))
        (db-copy (make-vector 3)))
    (db-set-filename! db-copy filename)
    (db-set-fields! db-copy (current-fields))
    (db-set-records! db-copy (make-selection-list))
    (write db-copy port)
    (close-output-port port)
    'selection-saved))

(define (merge-db filename field-name) 'merged)

(define test-db
  #("albums"
    (ARTIST TITLE YEAR BRIAN-LIKES?)
    (#((THE BEATLES) "A Hard Day's Night (original soundtrack)" 1964 #T)
     #((THE BEATLES) "Rubber Soul" 1965 #T)
     #((THE ZOMBIES) "Odessey and Oracle" 1967 #T)
     #((FRANK ZAPPA) "Hot Rats" 1970 #F)
     #((THE BILL FRISELL BAND) "Where in the World?" 1991 #F))))

