
; Shell scripts

; -------------------------------------------------------------

; Scheme script that displays 'Hello, World!' message

;; ":"; exec mzscheme -r $0 "$@"
;;
;; (display "Hello, World!")
;; (newline))

; -------------------------------------------------------------

; Script displaying its arguments stored in 'argv' vector

;; ":""; exec mzscheme -r $0 "$@"
;;
;; ; Put in argv-count the number of arguments supplied
;;
;; (define argv-count (vector-length argv))
;;
;; (let loop ((i 0))
;;   (unless (>= i argv-count)
;;     (display (vector-ref argv i))
;;     (newline)
;;     (loop (+ i 1))))

; -------------------------------------------------------------

; Script to split file into chunks

;; ":"";exec mzscheme -r $0 "$@"
;;
;; ; floppy-size = number of bytes that will comfortably fit on a
;; ;               3.5" floppy
;;
;; (define floppy-size 1440000)
;;
;; ; split splits the bigfile f into the smaller, floppy-sized
;; ; subfiles, viz., subfile-prefix.1, subfile-prefix.2, etc.
;;
;; (define split
;;   (lambda (f subfile-prefix)
;;     (call-with-input-file f
;;       (lambda (i)
;;         (let loop ((n 1))
;;           (if (copy-to-floppy-sized-subfile i subfile-prefix n)
;;               (loop (+ n 1))))))))
;;
;; ; copy-to-floppy-sized-subfile copies the next 1.44 million
;; ; bytes (if there are less than that many bytes left, it
;; ; copies all of them) from the big file to the nth
;; ; subfile.  Returns true if there are bytes left over,
;; ; otherwise returns false.
;;
;; (define copy-to-floppy-sized-subfile
;;   (lambda (i subfile-prefix n)
;;     (let ((nth-subfile (string-append subfile-prefix ".""
;;                                       (number->string n))))
;;       (if (file-exists? nth-subfile) (delete-file nth-subfile))
;;       (call-with-output-file nth-subfile
;;         (lambda (o)
;;           (let loop ((k 1))
;;             (let ((c (read-char i)))
;;               (cond ((eof-object? c) #f)
;;                     (else
;;                      (write-char c o)
;;                      (if (< k floppy-size)
;;                          (loop (+ k 1))
;;                          #t))))))))))
;;
;; ; bigfile = script’s first arg
;; ;         = the file that needs splitting
;;
;; (define bigfile (vector-ref argv 0))
;;
;; ; subfile-prefix = script’s second arg
;; ;                = the basename of the subfiles
;;
;; (define subfile-prefix (vector-ref argv 1))
;;
;; ; Call split, making subfile-prefix.{1,2,3,...} from
;; ; bigfile
;;
;; (split bigfile subfile-prefix)

; -------------------------------------------------------------
