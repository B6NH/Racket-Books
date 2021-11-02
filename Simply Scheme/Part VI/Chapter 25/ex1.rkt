; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

;; Implementation notes
;; Cells are stored as vector of vectors
;; They are selected by columns and rows
;; Cell id is list, e.g. (id 3 5)
;; Cell is vector of length 4
;; (value expression cells-it-depends-on cells-that-depend-on-it)

; -------------------------------------------------------------------

;; Navigation Commands
;; Up - p
;; Down - n
;; Left - b
;; Right - f
;; Select cell x - (select x)

;; Data commands (put value cell)
;; Set selected cell value to 8 - (put 8)
;; Set a6 to 5 - (put 5 a6)
;; Set all row 10 to 3 - (put 3 10)
;; Set all col d to 6 - (put 6 d)
;; Non-empty cell values arent modified during row/cell assignments

;; Other commands
;; Select top-left window cell - (window c4)
;; Set column f precision to 6 - (set-precision f 6)
;; Set column d width to 4 - (set-width d 4)
;; Undo last action - undo

; -------------------------------------------------------------------

;; Solution of exercise 1
(define total-cols 26)
(define total-rows 30) ;; Works with value 40

;; Main function
(define (spreadsheet)

  ;; Fill *the-spreadsheet-array* with rows, columns and cells
  (init-array)

  ;; Vector *special-cells* size is 2

  ;; Selected cell
  ;; Set (*special-cells* 0) to '(id 1 1)
  (set-selection-cell-id! (make-id 1 1))

  ;; Top left visible cell
  ;; Set (*special-cells* 1) to '(id 1 1)
  (set-screen-corner-cell-id! (make-id 1 1))

  ;; Process commands in loop or quit
  (command-loop))

(define (command-loop)

  ;; Display spreadsheet
  (print-screen)

  ;; Quit or process command
  ;; Commands are stored in *the-commands* list
  (let ((command-or-formula (read)))
    (if (equal? command-or-formula 'exit)
        "Bye!"
        (begin (process-command command-or-formula)
               (command-loop)))))

;; Process command or formula
(define (process-command command-or-formula)
  (cond
    ((and (list? command-or-formula)
          (command? (car command-or-formula))) ;; Is the first element a command?
     (execute-command command-or-formula)) ;; list - (f 3)
    ((command? command-or-formula) ;; Is the entire element a command?
     (execute-command (list command-or-formula 1))) ;; word - f
    (else (exhibit ;; formula, for example (* b4 a2)
            (ss-eval
              (pin-down command-or-formula
                        (selection-cell-id)))))))

;; Apply command, for example next-row to arguments
(define (execute-command command)
  (apply (get-command (car command))
         (cdr command)))

;; Show value and continue
(define (exhibit val)
  (show val)
  (show "Type RETURN to redraw screen")
  (read-line)
  (read-line))

(define (window cell-name)
  (begin
    (set! *history*
      (cons (list 'set-screen-corner-cell-id!
                   (screen-corner-cell-id))
            *history*))
    (set-screen-corner-cell-id! (cell-name->id cell-name))))

(define (set-prec-width column prec-width vec)
  (let ((indx (- (letter->number column) 1)))
    (begin
      (set! *history*
        (cons (list 'vector-set! vec indx (vector-ref vec indx))
              *history*))
      (vector-set! vec indx prec-width))))

(define (set-precision column precision)
  (set-prec-width column precision *column-precisions*))

(define (set-width column width)
  (set-prec-width column width *column-widths*))

(define (undo p)
  (if (null? *history*)
      'done
      (begin
        (apply (get-private-function (caar *history*)) (cdar *history*))
        (set! *history* (cdr *history*)))))

;;; Commands

;; Cell selection commands: F, B, N, P, and SELECT

(define (save-row-col f rc)
  (set! *history* (cons (list f rc) *history*)))

(define (save-row row)
  (save-row-col 'set-selected-row! row))

(define (save-col col)
  (save-row-col 'set-selected-column! col))

(define (prev-row delta)
  (let ((row (id-row (selection-cell-id))))
    (if (< (- row delta) 1)
        (error "Already at top.")
        (begin
          (save-row row)
          (set-selected-row! (- row delta))))))

(define (next-row delta)
  (let ((row (id-row (selection-cell-id))))
    (if (> (+ row delta) total-rows)
        (error "Already at bottom.")
        (begin
          (save-row row)
          (set-selected-row! (+ row delta))))))

(define (prev-col delta)
  (let ((col (id-column (selection-cell-id))))
    (if (< (- col delta) 1)
        (error "Already at left.")
        (begin
          (save-col col)
          (set-selected-column! (- col delta))))))

(define (next-col delta)
  (let ((col (id-column (selection-cell-id))))
    (if (> (+ col delta) total-cols)
        (error "Already at right.")
        (begin
          (save-col col)
          (set-selected-column! (+ col delta))))))

(define (set-selected-row! new-row)
  (select-id! (make-id (id-column (selection-cell-id)) new-row)))

(define (set-selected-column! new-column)
  (select-id! (make-id new-column (id-row (selection-cell-id)))))

(define (select-id! id)
  (set-selection-cell-id! id)

  ;; If newly selected cell is outside
  ;; screenset new corner cell
  (adjust-screen-boundaries))

(define (select cell-name)
  (begin
    (set! *history* (cons (list 'select-id! (selection-cell-id)) *history*))
    (select-id! (cell-name->id cell-name))))

;; Set new corner cell if necessary
(define (adjust-screen-boundaries)
  (let ((row (id-row (selection-cell-id)))
        (col (id-column (selection-cell-id))))
    (if (< row (id-row (screen-corner-cell-id)))
        (set-corner-row! row)
        'do-nothing)
    (if (>= row (+ (id-row (screen-corner-cell-id)) 20))
        (set-corner-row! (- row 19))
        'do-nothing)
    (if (< col (id-column (screen-corner-cell-id)))
        (set-corner-column! col)
        'do-nothing)
    (if (>= col (+ (id-column (screen-corner-cell-id)) 6))
        (set-corner-column! (- col 5))
        'do-nothing)))

(define (set-corner-row! new-row)
  (set-screen-corner-cell-id!
   (make-id (id-column (screen-corner-cell-id)) new-row)))

(define (set-corner-column! new-column)
  (set-screen-corner-cell-id!
   (make-id new-column (id-row (screen-corner-cell-id)))))


;; LOAD

;; Open file
(define (spreadsheet-load filename)
  (let ((port (open-input-file filename)))
    (sl-helper port)
    (close-input-port port)))

;; Read, show and process commands from file
(define (sl-helper port)
  (let ((command (read port)))
    (if (eof-object? command)
        'done
        (begin (show command)
               (process-command command)
               (sl-helper port)))))


(define (save-put id val)
  (set! *history*
    (cons (list 'put-expr
                (cell-expr id)
                 id)
          *history*))
  (put-formula-in-cell val id))

(define (save-affected index indices fn)
  (if (empty? indices)
      'done
      (set! *history*
        (cons (list fn index indices)
              *history*))))

(define (clear-in-row row col-nums)
  (if (empty? col-nums)
      'done
      (begin
        (put-expr '() (vector (car col-nums) row))
        (clear-in-row row (cdr col-nums)))))

(define (clear-in-col col row-nums)
  (if (empty? row-nums)
      'done
      (begin
        (put-expr '() (vector col (car row-nums)))
        (clear-in-col col (cdr row-nums)))))

;; PUT

(define (put formula . where) ;; optional argument
 (begin
   (cond

      ;; Currently selected cell
      ;; Parameter where is empty list
      ((null? where)
       (save-put (selection-cell-id) formula))

      ;; Cell whose name was passed as an argument
      ;; Parameter where is non-empty list
      ;; Its first element is cell name
      ((cell-name? (car where))
       (let ((cid (cell-name->id (car where))))
         (save-put cid formula)))

      ;; Row
      ((number? (car where))
       (let ((cw (car where)))
         (save-affected cw (put-all-cells-in-row formula cw) 'clear-in-row)))

      ;; Column
      ((letter? (car where))
       (let ((indx (letter->number (car where))))
         (save-affected indx (put-all-cells-in-col formula indx) 'clear-in-col)))

      ;; Error
      (else (error "Put it where?")))

   ;; Show number of modified cells
   (show-line (se *modified-cells* "cells modified"))

   ;; Reset modified cells counter
   (set! *modified-cells* 0)))

(define (put-all-cells-in-row formula row)
  (put-all-helper formula (lambda (col) (make-id col row)) 1 total-cols '()))

(define (put-all-cells-in-col formula col)
  (put-all-helper formula (lambda (row) (make-id col row)) 1 total-rows '()))

;; Put formula in row or col
(define (put-all-helper formula id-maker this max acc)
  (if (> this max)
      acc
      ;; Function id-maker has col or row index captured inside
      ;; Second value is passed as parameter 'this' starting with row/col 1
      ;; Increment row or col value
      (put-all-helper
        formula id-maker (+ 1 this) max
        (if (try-putting formula (id-maker this))
            (cons this acc)
            acc))))

;; Put formula in empty cell
;; Don't modify other cells
(define (try-putting formula id)
  (if (or (null? (cell-value id)) (null? formula))
      (put-formula-in-cell formula id)
      #false))

;; Pin down formula and put it in cell
(define (put-formula-in-cell formula id)
  (put-expr (pin-down formula id) id))


;;; The Association List of Commands

;; Check if it is command name
(define (command? name)
  (assoc name *the-commands*))

;; Get command, for example 'n -> next-row
(define (get-command name)
  (let ((result (assoc name *the-commands*)))
    (if (not result)
        #f
        (cadr result))))

(define *the-commands*
  (list (list 'p prev-row)
        (list 'n next-row)
        (list 'b prev-col)
        (list 'f next-col)
        (list 'select select)
        (list 'put put)
        (list 'window window)
        (list 'set-precision set-precision)
        (list 'set-width set-width)
        (list 'undo undo)
        (list 'load spreadsheet-load)))

;;; Pinning Down Formulas Into Expressions

;; Modify formula to contain absolute cell values
;; Parameter id is reference cell
;; For example (pin-down '(* (cell b) (cell c)) 'd4) -> (* (id 2 4) (id 3 4))
(define (pin-down formula id)
  (cond

    ;; Convert cell name to id
    ((cell-name? formula) (cell-name->id formula))

    ;; Leave word unmodified
    ((word? formula) formula)

    ;; Empty list for null
    ((null? formula) '())

    ;; Pin down cell values
    ((equal? (car formula) 'cell)
     (pin-down-cell (cdr formula) id))

    ((equal? (car formula) 'accumulate)
     (accumulate-cells (cdr formula)))

    ;; Recursively pin-down subformulas and check bounds
    (else
      (bound-check
        (map (lambda (subformula) (pin-down subformula id)) formula)))))

;; Check if any of subformulas is out of bounds
(define (bound-check form)
  (if (member 'out-of-bounds form)
      'out-of-bounds
      form))

;; Set absolute cell address values
(define (pin-down-cell args reference-id)
  (cond

    ;; Error (no cell arguments)
    ((null? args) (error "Bad cell specification: (cell)"))

    ;; Only row (number) or col (letter)
    ((null? (cdr args))
     (cond ((number? (car args))
            (make-id (id-column reference-id) (car args))) ;; Row
           ((letter? (car args))
            (make-id (letter->number (car args)) ;; Col
                     (id-row reference-id)))
           (else
            (error "Bad cell specification:" (cons 'cell args))))) ;; Error

    ;; Select specific cell (col and row)
    (else

      ;; Col and row can be shifted with > and < symbols
      ;; Use function pin-down-col and pin-down-row to find final values
      (let ((col (pin-down-col (car args) (id-column reference-id)))
            (row (pin-down-row (cadr args) (id-row reference-id))))

        ;; Check if values are in bounds
        (if (and (>= col 1) (<= col total-cols) (>= row 1) (<= row total-rows))
            (make-id col row)
            'out-of-bounds)))))

;; Find target column
(define (pin-down-col new old)
  (cond

    ;; Same column
    ((equal? new '*) old)

    ;; Shift right or left by some number of columns
    ((equal? (first new) '>) (+ old (bf new)))
    ((equal? (first new) '<) (- old (bf new)))

    ;; Exact column
    ((letter? new) (letter->number new))

    ;; Column error
    (else (error "What column?"))))

;; Find target row
(define (pin-down-row new old)
  (cond

    ;; Exact row
    ((number? new) new)

    ;; Same column
    ((equal? new '*) old)

    ;; Shift down or up by some number of rows
    ((equal? (first new) '>) (+ old (bf new)))
    ((equal? (first new) '<) (- old (bf new)))

    ;; Row error
    (else (error "What row?"))))

;;; Dependency Management

;; Put pinned down formula (now expression) in cell
(define (put-expr expr-or-out-of-bounds id)

  ;; Set expression value
  (let ((expr (if (equal? expr-or-out-of-bounds 'out-of-bounds)
                  '()
                  expr-or-out-of-bounds)))

    ;; Remove current cell id from parent cells
    (for-each (lambda (old-parent)
                (set-cell-children!
                  old-parent
                  (remove id (cell-children old-parent))))
              (cell-parents id))

    ;; Set current cell expression
    (set-cell-expr! id expr)

    ;; Set parent ids in current cell
    ;; Function extract-ids returns all cell ids from expression
    (set-cell-parents! id (remdup (extract-ids expr)))

    ;; Add this cell id to its new parents children lists
    (for-each (lambda (new-parent)
                (set-cell-children!
                  new-parent
                  (cons id (cell-children new-parent))))
              (cell-parents id))

    ;; Expression may or may not compute a new value for this cell
    (figure id)))

;; Get ids from expression and ignore other values
(define (extract-ids expr)
  (cond

    ;; Base cases
    ((id? expr) (list expr))
    ((word? expr) '())
    ((null? expr) '())

    ;; Recursive step for compound expressions
    (else (append (extract-ids (car expr))
                  (extract-ids (cdr expr))))))


(define (figure id)
  (cond

    ;; Clear cell for empty expression
    ((null? (cell-expr id)) (setvalue id '()))

    ;; Compute and save new value if all
    ;; parents already have numerical values
    ((all-evaluated? (cell-parents id))
     (setvalue id (ss-eval (cell-expr id))))

    ;; If any parent doesn't have
    ;; numerical value clear this cell
    (else (setvalue id '()))))

;; Check whether all cells have numerical values
(define (all-evaluated? ids)
  (cond
    ((null? ids) #t)
    ((not (number? (cell-value (car ids)))) #f)
    (else (all-evaluated? (cdr ids)))))

(define (setvalue id value)

  ;; Remember old value and set the new one
  (let ((old (cell-value id)))
    (set-cell-value! id value)

    ;; If value changed all children must be figured
    (if (not (equal? old value))
        (for-each figure (cell-children id))
        'do-nothing)))


;;; Evaluating Expressions

;; Evaluate expression
(define (ss-eval expr)
  (cond

    ;; Don't modify numbers
    ((number? expr) expr)

    ;; Get quoted expression value
    ((quoted? expr) (quoted-value expr))

    ;; Get cell value
    ((id? expr) (cell-value expr))

    ;; Apply function to recursively evaluated arguments
    ((invocation? expr)
     (apply (get-function (car expr))
            (map ss-eval (cdr expr))))

    (else (error "Invalid expression:" expr))))

(define (quoted? expr)
  (or (string? expr)
      (and (list? expr) (equal? (car expr) 'quote))))

(define (quoted-value expr)
  (if (string? expr)
      expr
      (cadr expr)))

(define (invocation? expr)
  (list? expr))

(define (get-private-function name)
  (cadr (assoc name *private-functions*)))

;; Get function, for example 'tan -> tan
(define (get-function name)
  (let ((result (assoc name *the-functions*)))
    (if (not result)
        (error "No such function: " name)
        (cadr result))))

(define *the-functions*
  (list (list '* *)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list 'abs abs)
        (list 'acos acos)
        (list 'asin asin)
        (list 'atan atan)
        (list 'ceiling ceiling)
        (list 'cos cos)
        (list 'count count)
        (list 'exp exp)
        (list 'expt expt)
        (list 'floor floor)
        (list 'gcd gcd)
        (list 'lcm lcm)
        (list 'log log)
        (list 'max max)
        (list 'min min)
        (list 'modulo modulo)
        (list 'quotient quotient)
        (list 'remainder remainder)
        (list 'round round)
        (list 'sin sin)
        (list 'sqrt sqrt)
        (list 'tan tan)
        (list 'truncate truncate)))

;;; Printing the Screen

(define (print-screen)

  ;; Print 3 newlines above spreadsheet
  (newline)
  (newline)
  (newline)

  ;; Pass visible cell column id to show-column-labels
  ;; function and show all labels
  (show-column-labels (id-column (screen-corner-cell-id)))

  ;; Display rows starting from corner cell
  (show-rows 20
    (id-column (screen-corner-cell-id))
    (id-row (screen-corner-cell-id)))

  ;; Display selected cell name at the botoom
  (display-cell-name (selection-cell-id))
  (display ":  ")

  ;; Selected cell value
  (show (cell-value (selection-cell-id)))

  ;; Show expression value from selected cell
  (display-expression (cell-expr (selection-cell-id)))
  (newline)

  ;; Display prompt
  (display "?? "))

(define (display-cell-name id)
  (display (number->letter (id-column id)))
  (display (id-row id)))

(define (show-column-labels col-number)

  ;; Blank before column labels
  (display "  ")

  ;; Display labels
  (show-label 6 col-number)

  ;; End displaying labels with newline
  (newline))

;; Display all labels
(define (show-label to-go this-col-number)
  (cond
    ((= to-go 0) '())
    (else

      (display "  -----")

      ;; Column letter
      (display (number->letter this-col-number))

      (display "----")

      ;; Recursive call
      (show-label (- to-go 1) (+ 1 this-col-number)))))


;; Show all rows
(define (show-rows to-go col row)
  (cond
    ((= to-go 0) 'done)
    (else

      ;; Align row
      (display (align row 2 0))
      (display " ")

      ;; Display 1 row
      (show-row 6 col row)
      (newline)

      ;; Display remaining rows
      (show-rows (- to-go 1) col (+ row 1)))))

(define (show-row to-go col row)
  (cond
    ((= to-go 0) 'done)
    (else
      (let ((col-index (- col 1)))

        ;; Display selection symbol
        (display (if (selected-indices? col row) ">" " "))

        ;; Display cell value
        (display-value (cell-value-from-indices col row)
                       (vector-ref *column-precisions* col-index)
                       (vector-ref *column-widths* col-index))

        ;; Ending selection symbol
        (display (if (selected-indices? col row) "<" " "))
        (show-row (- to-go 1) (+ 1 col) row)))))

;; Check if this cell is selected
(define (selected-indices? col row)
  (and (= col (id-column (selection-cell-id)))
       (= row (id-row (selection-cell-id)))))

;; Display aligned value
(define (display-value val precision width)
  (display (align (if (null? val) "" val) width precision)))

;; Display cell names instead of ids
(define (display-expression expr)
  (cond
    ((null? expr) (display '()))
    ((quoted? expr) (display (quoted-value expr)))
    ((word? expr) (display expr))
    ((id? expr)
     (display-cell-name expr))
    (else (display-invocation expr))))

;; Display expression and subexpressions
(define (display-invocation expr)
  (display "(")
  (display-expression (car expr))
  (for-each (lambda (subexpr)
              (display " ")
              (display-expression subexpr))
            (cdr expr))
  (display ")"))


;;; Abstract Data Types

;; Special cells: the selected cell and the screen corner

(define *special-cells* (make-vector 2))

(define (selection-cell-id)
  (vector-ref *special-cells* 0))

(define (set-selection-cell-id! new-id)
  (vector-set! *special-cells* 0 new-id))

(define (screen-corner-cell-id)
  (vector-ref *special-cells* 1))

(define (set-screen-corner-cell-id! new-id)
  (vector-set! *special-cells* 1 new-id))

(define *private-functions*
  (list
    (list 'set-selected-row! set-selected-row!)
    (list 'set-selected-column! set-selected-column!)
    (list 'select-id! select-id!)
    (list 'vector-set! vector-set!)
    (list 'put-expr put-expr)
    (list 'clear-in-row clear-in-row)
    (list 'clear-in-col clear-in-col)
    (list 'set-screen-corner-cell-id! set-screen-corner-cell-id!)))

;; Cell names

(define (cell-name? expr)
  (and (word? expr)
       (letter? (first expr))
       (number? (bf expr))))

(define (cell-name-column cell-name)
  (letter->number (first cell-name)))

(define (cell-name-row cell-name)
  (bf cell-name))

;; For example translate 'b4 -> '(id 2 4)
(define (cell-name->id cell-name)
  (make-id (cell-name-column cell-name)
           (cell-name-row cell-name)))

;; Cell IDs

(define (make-id col row)
  (vector col row))

;; Id selectors

(define (id-column id)
  (vector-ref id 0))

(define (id-row id)
  (vector-ref id 1))

(define (id? x)
  (and (vector? x)
       (= (vector-length x) 2)))

;; Cells

(define (make-cell)
  (vector '() '() '() '()))


;; Cell selectors and mutators

(define (cell-value id)
  (vector-ref (cell-structure id) 0))

(define (cell-value-from-indices col row)
  (vector-ref (cell-structure-from-indices col row) 0))

(define (cell-expr id)
  (vector-ref (cell-structure id) 1))

(define (cell-parents id)
  (vector-ref (cell-structure id) 2))

(define (cell-children id)
  (vector-ref (cell-structure id) 3))

(define (set-cell-value! id val)
  (begin
    (vector-set! (cell-structure id) 0 val)
    (set! *modified-cells* (+ *modified-cells* 1))))

(define (set-cell-expr! id val)
  (vector-set! (cell-structure id) 1 val))

(define (set-cell-parents! id val)
  (vector-set! (cell-structure id) 2 val))

(define (set-cell-children! id val)
  (vector-set! (cell-structure id) 3 val))

;; Get selected cell
(define (cell-structure id)
  (global-array-lookup (id-column id)
                       (id-row id)))

(define (cell-structure-from-indices col row)
  (global-array-lookup col row))

;; Main array
(define *the-spreadsheet-array* (make-vector (* total-rows total-cols)))

(define *modified-cells* 0)

(define *column-precisions* (make-vector total-cols 2))

(define *column-widths* (make-vector total-cols 10))

(define *history* '())

;; Get valid cell at col-row
(define (global-array-lookup col row)
  (if (and (<= row total-rows) (<= col total-cols))
      (vector-ref *the-spreadsheet-array* (- (+ (* (- row 1) total-cols) col) 1))
      (error "Out of bounds")))

;; Get cell (old version)
;; (define (global-array-lookup col row)
;;   (if (and (<= row total-rows) (<= col total-cols))
;;       ;; Indices start at 0
;;       (vector-ref
;;         (vector-ref *the-spreadsheet-array* (- row 1))
;;         (- col 1))
;;       (error "Out of bounds")))

;; Initialize array (old version)
;; (define (init-array)
;;   (fill-array-with-rows (- total-rows 1)))

;; Initialize all cells
(define (init-array)
  (fill-row-with-cells *the-spreadsheet-array* (- (* total-cols total-rows) 1)))

;; Create rows (old version)
;;(define (fill-array-with-rows n)
;;  (if (< n 0)
;;      'done
;;      (begin
;;        (vector-set! *the-spreadsheet-array* n (make-vector total-cols))
;;        (fill-row-with-cells
;;          (vector-ref *the-spreadsheet-array* n) (- total-cols 1))
;;        (fill-array-with-rows (- n 1)))))

;; Create cells in row
(define (fill-row-with-cells vec n)
  (if (< n 0)
      'done
      (begin
        (vector-set! vec n (make-cell)) ;; create single cell
        (fill-row-with-cells vec (- n 1)))))

;;; Utility Functions

(define alphabet
  '#(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define (letter? something)
  (and (word? something)
       (= 1 (count something))
       (vector-member something alphabet)))

(define (number->letter num)
  (vector-ref alphabet (- num 1)))

(define (letter->number letter)
  (+ (vector-member letter alphabet) 1))

(define (vector-member thing vector)
  (vector-member-helper thing vector 0))

(define (vector-member-helper thing vector index)
  (cond
    ((= index (vector-length vector)) #f)
    ((equal? thing (vector-ref vector index)) index)
    (else (vector-member-helper thing vector (+ 1 index)))))

(define (remdup lst)
  (cond
    ((null? lst) '())
    ((member (car lst) (cdr lst))
     (remdup (cdr lst)))
    (else (cons (car lst) (remdup (cdr lst))))))

(define (remove bad-item lst)
  (filter (lambda (item) (not (equal? item bad-item)))
           lst))

;;; Accumulate

(define (accumulate-cells cells)
  (let ((symbol (car cells))
        (start-col (first (cadr cells)))
        (end-col (first (caddr cells)))
        (start-row (last (cadr cells)))
        (end-row (last (caddr cells))))
    (cons symbol (expand-rows (letter->number start-col) (letter->number end-col) start-row end-row))))

(define (expand-rows start-column-index end-column-index current-row end-row)
  (if (> current-row end-row)
      '()
      (append (expand-row current-row start-column-index end-column-index)
              (expand-rows start-column-index end-column-index (+ current-row 1) end-row))))

(define (expand-row row-number current-column-index end-column-index)
  (if (> current-column-index end-column-index)
      '()
      (cons (vector current-column-index row-number)
            (expand-row row-number (+ current-column-index 1) end-column-index))))

; -------------------------------------------------------------------

(spreadsheet)
