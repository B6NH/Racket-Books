
; CGI scripts

; -------------------------------------------------------------

; Introductory line for CGI script (mzscheme)

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0 "$@"

; -------------------------------------------------------------

; Script that displays environment variables as plain text
; First line of its output is 'content-type: text/plain'

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0 "$@"
;;
;; ;Identify content-type as plain text.
;;
;; (display "content-type: text/plain"") (newline)
;; (newline)
;;
;; ;Generate a page with the requested info.  This is
;; ;done by simply writing to standard output.
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

; HTML to display selected environment variable
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

;; #!/bin/sh
;; ":"";exec /usr/local/bin/mzscheme -r $0 "$@"
;;
;; (display "content-type: text/plain"") (newline)
;; (newline)
;;
;; ; string-index returns the leftmost index in string s
;; ; that has character c
;;
;; (define string-index
;;   (lambda (s c)
;;     (let ((n (string-length s)))
;;       (let loop ((i 0))
;;         (cond ((>= i n) #f)
;;               ((char=? (string-ref s i) c) i)
;;               (else (loop (+ i 1))))))))
;;
;; ; split breaks string s into substrings separated by character c
;;
;; (define split
;;   (lambda (c s)
;;     (let loop ((s s))
;;       (if (string=? s """) '()
;;           (let ((i (string-index s c)))
;;             (if i (cons (substring s 0 i)
;;                         (loop (substring s (+ i 1)
;;                                          (string-length s))))
;;                 (list s)))))))
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
