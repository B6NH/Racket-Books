
; Input & Output

(begin

  (define line (lambda () (display "----------------\n")))
  (define out-name "greeting.txt")
  (define in-name "hi.txt")

  (display 14 (current-output-port)) (newline)
  (write 15) (newline)

  (line)

  (display "16") (newline)
  (write "17") (newline)

  (line)

  (display #\A) (newline)
  (write #\B ) (newline)

  (line)

  (define i (open-input-file in-name))
  (define oneChar (read-char i))
  (define restFile (read i))
  (close-input-port i)

  (write oneChar) (newline)
  (write restFile) (newline)

  (line)

  (define o (open-output-file out-name))
  (display "hello" o) (write-char #\space o)
  (display 'world o) (newline o)
  (close-output-port o)

  (write
    (call-with-input-file out-name
      (lambda (i)
        (let* ((a (read-char i))
               (b (read-char i))
               (c (read-char i)))
          (list b c a)))))
  (newline)

  (line)

  (define is (open-input-string "string ports"))

  (display (read is)) (newline)
  (display (read is)) (newline)

  (line)

  (define os (open-output-string))
  (write 'ola os) (write-char #\, os)
  (display " " os) (display "mundo" os)

  (display (get-output-string os)) (newline)

  (line))

