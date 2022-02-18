
; System Interface

(define file-name "aaa.txt")

(define show-all-helper
  (lambda (p)
    (let ((l (read-line p)))
      (if (not (eof-object? l))
          (begin
            (display (string-append "  " l)) (newline) (show-all-helper p))))))

(define show-all-files
  (lambda ()
    (let ((p (open-process (list path: "ls"))))
      (show-all-helper p)
      (close-port p))))

(define create-file
  (lambda (name)
    (let ((p (open-process (list path: "touch" arguments: (list name)))))
      (close-port p))))


(begin

  (display
    (and (file-exists? "enter.scm")
         (not (file-exists? "abc.scm"))))
  (newline)

  (create-file file-name)

  (display "Files:\n")
  (show-all-files)

  (delete-file file-name)

  (display (not (file-exists? file-name))) (newline)

  (display (string-append "HOME: " (getenv "HOME"))) (newline)
  (display (string-append "SHELL: " (getenv "SHELL"))) (newline))
