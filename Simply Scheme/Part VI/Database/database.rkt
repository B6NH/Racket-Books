; Database

;;#lang racket
(require (planet dyoo/simply-scheme:2:2))

;; Load program in racket interactive mode
;; (load "database.rkt")

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

(define (get-record)
  (get-record-loop 0
    (make-vector (length (current-fields)))
    (current-fields)))

(define (get-record-loop which-field record fields)
  (if (null? fields)
      record
      (begin
        (display "Value for ")
        (display (car fields))
        (display "--> ")
        (vector-set! record which-field (read))
        (get-record-loop (+ which-field 1) record (cdr fields)))))

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
        (show-record-helper (cdr fields) record (+ index 1)))))

;; Display list of records
(define (show-records fields records)
  (show-records-helper fields records 1))

(define (show-records-helper fields records index)
  (if (empty? records)
      'listed
      (begin
        (show (string-append "RECORD " (number->string index)))
        (show-record fields (car records))
        (show-records-helper fields (cdr records) (+ index 1)))))

;; List current database records
(define (list-db)
  (show-records (current-fields) (current-records)))

;; Get record at index
(define (get-record-at index records)
  (get-record-at-helper index records 1))

(define (get-record-at-helper index records current-index)
  (if (= index current-index)
      (car records)
      (get-record-at-helper index (cdr records) (+ current-index 1))))

;; Get record from current database records
(define (get-current-record-at index)
  (get-record-at index (current-records)))

(define (get-field-index field fields)
  (get-field-index-helper field fields 0))

(define (get-field-index-helper field fields index)
  (if (equal? field (car fields))
      index
      (get-field-index-helper field (cdr fields) (+ index 1))))

(define (get-current-field-index field)
  (get-field-index field (current-fields)))

(define (show-record-with-fields record)
  (show-record (current-fields) record))

(define (edit-record index)
  (let ((rec (get-current-record-at index)))
    (show-record-with-fields rec)
    (display "Edit which field? ")
    (let ((field (read)))
      (display "New value for ")
      (display field)
      (vector-set! rec (get-current-field-index field) (read))
      (show-record-with-fields rec))))

(define test-db
  #("albums"
    (ARTIST TITLE YEAR BRIAN-LIKES?)
    (#((THE BEATLES) "A Hard Day's Night (original soundtrack)" 1964 #T)
     #((THE BEATLES) "Rubber Soul" 1965 #T)
     #((THE ZOMBIES) "Odessey and Oracle" 1967 #T)
     #((FRANK ZAPPA) "Hot Rats" 1970 #F)
     #((THE BILL FRISELL BAND) "Where in the World?" 1991 #F))))

