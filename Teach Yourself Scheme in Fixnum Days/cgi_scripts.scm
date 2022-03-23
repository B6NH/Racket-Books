
; CGI scripts

; -------------------------------------------------------------

; Introductory line for CGI script (mzscheme)

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0 "$@"

; -------------------------------------------------------------

; Script that displays environment variables as plain text
; First line of its output is 'content-type: text/plain' and blank line

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0 "$@"
;;
;; ; Identify content-type as plain text.
;;
;; (display "content-type: text/plain"") (newline)
;; (newline)
;;
;; ; Generate a page with the requested info.  This is
;; ; done by simply writing to standard output (display function).
;;
;; (for-each
;;  (lambda (env-var)
;;    (display env-var)
;;    (display " = "")
;;    (display (or (getenv env-var) """))
;;    (newline))
;;  '("AUTH_TYPE""
;;    "CONTENT_LENGTH""
;;    "CONTENT_TYPE""
;;    "DOCUMENT_ROOT""
;;    "GATEWAY_INTERFACE""
;;    "HTTP_ACCEPT""
;;    "HTTP_REFERER"" ; [sic]
;;    "HTTP_USER_AGENT""
;;    "PATH_INFO""
;;    "PATH_TRANSLATED""
;;    "QUERY_STRING""
;;    "REMOTE_ADDR""
;;    "REMOTE_HOST""
;;    "REMOTE_IDENT""
;;    "REMOTE_USER""
;;    "REQUEST_METHOD""
;;    "SCRIPT_NAME""
;;    "SERVER_NAME""
;;    "SERVER_PORT""
;;    "SERVER_PROTOCOL""
;;    "SERVER_SOFTWARE""))

; -------------------------------------------------------------

; HTML form to display selected environment variable
; Information is sent via the environment variable called QUERY_STRING

;; <html>
;; <head>
;; <title>Form for checking environment variables</title>
;; </head>
;; <body>
;;
;; <form method=get
;;       action="http://www.foo.org/cgi-bin/testcgi2.scm">
;; Enter environment variable: <input type=text name=envvar size=30>
;; <p>
;;
;; <input type=submit>
;; </form>
;;
;; </body>
;; </html>

; -------------------------------------------------------------

; Script to extract information from QUERY_STRING and output answer page
; Data is formatted as sequence of parameter/argument pairs separated by '&' character
; Parameter is separated from argument by '=' character

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0 "$@"
;;
;; (display "content-type: text/plain"") (newline)
;; (newline)
;;
;; ; string-index returns the leftmost index in string s
;; ; that has character c
;;
(define string-index
  (lambda (s c)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (cond ((>= i n) #f)
              ((char=? (string-ref s i) c) i)
              (else (loop (+ i 1))))))))
;;
;; ; split breaks string s into substrings separated by character c
;;
(define split
  (lambda (c s)
    (let loop ((s s))
      (if (string=? s "") '()
          (let ((i (string-index s c)))
            (if i (cons (substring s 0 i)
                        (loop (substring s (+ i 1)
                                         (string-length s))))
                (list s)))))))
;;
;; (define args
;;   (map (lambda (par-arg)
;;          (split #\= par-arg))
;;        (split #\& (getenv "QUERY_STRING""))))
;;
;; (define envvar (cadr (assoc "envvar"" args)))
;;
;; (display envvar)
;; (display " = "")
;; (display (getenv envvar))
;;
;; (newline)

; -------------------------------------------------------------

; Display environment variable using library

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0 "$@"
;;
;; ; Load the cgi utilities
;;
;; (load-relatve "cgi.scm"")
;;
;; (display "content-type: text/plain"") (newline)
;; (newline)
;;
;; ; Read the data input via the form
;;
;; (parse-form-data)
;;
;; ; Get the envvar parameter
;;
;; (define envvar (form-data-get/1 "envvar"))
;;
;; ; Display the value of the envvar
;;
;; (display envvar)
;; (display " = "")
;; (display (getenv envvar))
;; (newline)

; -------------------------------------------------------------

; Define global table in library file

;; ; Load our table definitions
;;
;; (load-relative "table.scm"")
;;
;; ; Define the *form-data-table*
;;
;; (define *form-data-table* (make-table 'equ string=?))

; -------------------------------------------------------------

; Read data supplied by user via form and store it in table
; One table parameter can contain multiple values

(define parse-form-data
  (lambda ()
    ((if (string-ci=? (or (getenv "REQUEST_METHOD") "GET")
                      "GET")
         parse-form-data-using-query-string
         parse-form-data-using-stdin))))

; -------------------------------------------------------------

; Parse query string and store form data in global *form‑data‑table*

(define parse-form-data-using-query-string
  (lambda ()
    (let ((query-string (or (getenv "QUERY_STRING") "")))
      (for-each
       (lambda (par=arg)
         (let ((par/arg (split #\= par=arg)))
           (let ((par (url-decode (car par/arg)))
                 (arg (url-decode (cadr par/arg))))
             (table-put!
              *form-data-table* par
              (cons arg
                    (table-get *form-data-table* par '()))))))
       (split #\& query-string)))))

; -------------------------------------------------------------

; Parse post data and store it in *form‑data‑table*

(define parse-form-data-using-stdin
  (lambda ()
    (let* ((content-length (getenv "CONTENT_LENGTH"))
           (content-length
             (if content-length
                 (string->number content-length) 0))
           (i 0))

      ; Parameter loop
      (let par-loop ((par '()))

        ; Read parameter character
        (let ((c (read-char)))
          (set! i (+ i 1))
          (if (or (> i content-length)
                  (eof-object? c) (char=? c #\=))
              (let arg-loop ((arg '()))

                ; Read argument character
                (let ((c (read-char)))
                  (set! i (+ i 1))
                  (if (or (> i content-length)
                          (eof-object? c) (char=? c #\&))

                      ; Decode parameter and argument
                      (let ((par (url-decode
                                   (list->string
                                     (reverse! par))))
                            (arg (url-decode
                                   (list->string
                                     (reverse! arg)))))

                        ; Put data in table
                        (table-put! *form-data-table* par
                          (cons arg
                                (table-get *form-data-table*
                                  par '())))

                        ; Next parameter
                        (unless (or (> i content-length)
                                    (eof-object? c))
                          (par-loop '())))

                      ; Cons argument character
                      (arg-loop (cons c arg)))))

              ; Cons parameter character
              (par-loop (cons c par))))))))

; -------------------------------------------------------------

; Decode url string

(define url-decode
  (lambda (s)
    (let ((s (string->list s)))
      (list->string
       (let loop ((s s))
         (if (null? s) '()
             (let ((a (car s)) (d (cdr s)))
               (case a
                 ((#\+) (cons #\space (loop d)))
                 ((#\%) (cons (hex->char (car d) (cadr d))
                              (loop (cddr d))))
                 (else (cons a (loop d)))))))))))

(define hex->char
  (lambda (x y)
    (integer->char
     (string->number (string x y) 16))))

; -------------------------------------------------------------

; Get all values assigned to a parameter

(define form-data-get
  (lambda (k)
    (table-get *form-data-table* k '())))

; Get first parameter value

(define form-data-get/1
  (lambda (k . default)
    (let ((vv (form-data-get k)))
      (cond ((pair? vv) (car vv))
            ((pair? default) (car default))
            (else "")))))

; -------------------------------------------------------------

; Display HTML

(define display-html
  (lambda (s . o)
    (let ((o (if (null? o) (current-output-port)
                 (car o))))
      (let ((n (string-length s)))
        (let loop ((i 0))
          (unless (>= i n)
            (let ((c (string-ref s i)))
              (display
               (case c
                 ((#\<) "&lt;")
                 ((#\>) "&gt;")
                 ((#\") "&quot;")
                 ((#\&) "&amp;")
                 (else c)) o)
              (loop (+ i 1)))))))))

; -------------------------------------------------------------

; Calculator script

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0
;;
;; ;Load the CGI utilities
;; (load-relative "cgi.scm"")
;;
;; (define uhoh #f)
;;
;; (define calc-eval
;;   (lambda (e)
;;     (if (pair? e)
;;         (apply (ensure-operator (car e))
;;                (map calc-eval (cdr e)))
;;         (ensure-number e))))
;;
;; (define ensure-operator
;;   (lambda (e)
;;     (case e
;;       ((+) +)
;;       ((-) -)
;;       ((*) *)
;;       ((/) /)
;;       ((**) expt)
;;       (else (uhoh "unpermitted operator"")))))
;;
;; (define ensure-number
;;   (lambda (e)
;;     (if (number? e) e
;;         (uhoh "non-number""))))
;;
;; (define print-form
;;   (lambda ()
;;     (display "<form action=\""")
;;     (display (getenv "SCRIPT_NAME""))
;;     (display "\">
;;   Enter arithmetic expression:<br>
;;   <input type=textarea name=arithexp><p>
;;   <input type=submit value=\"Evaluate\">
;;   <input type=reset value=\"Clear\">
;; </form>"")))
;;
;; (define print-page-begin
;;   (lambda ()
;;     (display "content-type: text/html
;;
;; <html>
;;   <head>
;;     <title>A Scheme Calculator</title>
;;   </head>
;;   <body>"")))
;;
;; (define print-page-end
;;   (lambda ()
;;     (display "</body>
;; </html>"")))
;;
;; (parse-form-data)
;;
;; (print-page-begin)
;;
;; (let ((e (form-data-get "arithexp"")))
;;   (unless (null? e)
;;     (let ((e1 (car e)))
;;       (display-html e1)
;;       (display "<p>
;;   =&gt;&nbsp;&nbsp;"")
;;       (display-html
;;        (call/cc
;;         (lambda (k)
;;           (set! uhoh
;;                 (lambda (s)
;;                   (k (string-append "Error: "" s))))
;;           (number->string
;;            (calc-eval (read (open-input-string (car e))))))))
;;       (display "<p>""))))
;;
;; (print-form)
;; (print-page-end)

; -------------------------------------------------------------

(display "HTML: ")
(display-html "abc <>\"& def")
(newline)

(begin
  (display
    (and
      (= 4 (string-index "abcdef" #\e))
      (equal? (split #\& "abc&def&ghi&jkl")
              '("abc" "def" "ghi" "jkl"))
      (equal? (url-decode "20%25+%2b+30%25+%3d+50%25%2c+%26c%2e")
              "20% + 30% = 50%, &c.")
      (equal? (hex->char #\2 #\5) #\%)
      (equal? (hex->char #\2 #\b) #\+)
      (equal? (hex->char #\3 #\d) #\=)
      (equal? (hex->char #\2 #\c) #\,)
      (equal? (hex->char #\2 #\6) #\&)
      (equal? (hex->char #\2 #\e) #\.)))
  (newline))
