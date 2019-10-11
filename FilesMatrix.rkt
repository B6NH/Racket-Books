;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Files) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
;;(read-words/line "ttt.txt")

; An LLS is one of: 
; – '()
; – (cons Los LLS)
; interpretation a list of lines, each is a list of Strings
 
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
 
; LLS -> List-of-numbers
; determines the number of words on each line 
 
(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1) (cons 2 (cons 0 '())))
 
(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else (cons (length (first lls))
                (words-on-line (rest lls)))]))
;; Figure 68: Counting the words on a line
;; ...

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))

;; (file-statistic "ttt.txt")

;; ----------------------------------------------
;; Exercise 172

(define(collapse-line line)
  (cond
    ((empty? line)"")
    (else
     (if(empty?(rest line))
        (string-append(first line)(collapse-line(rest line)))
        (string-append
         (first line)
         " "
         (collapse-line(rest line)))))))

;; list-of-lines -> string
(define(collapse lol)
  (cond
    ((empty? lol)"")
    (else(string-append(collapse-line(first lol))"\n"
                       (collapse(rest lol))))))


(define line2(list "jeszcze" "polska"))
(define line3(list "nie" "zginela"))
(define sample(cons line2(cons line3 '())))

(check-expect(collapse sample)"jeszcze polska\nnie zginela\n")
(check-expect(collapse-line line3)"nie zginela")

;;(write-file "ttt.dat"(collapse (read-words/line "ttt.txt")))

;; ----------------------------------------------
;; Exercise 173

;;(read-words/line "ttt.txt")

(define(remove-articles-line line)
  (cond
    ((empty? line)'())
    (else(if(or(string=?(first line)"a")
               (string=?(first line)"an")
               (string=?(first line)"the"))
            (remove-articles-line(rest line))
            (cons(first line)(remove-articles-line(rest line)))))))


(define(remove-articles sometext)
  (cond
    ((empty? sometext)'())
    (else(cons(remove-articles-line(first sometext))
              (remove-articles(rest sometext))))))

(define line4(list "jeszcze" "an" "polska"))
(define line5(list "nie" "the" "zginela"))
(define sample2(cons line4(cons line5 '())))

(check-expect(remove-articles-line line4)(list "jeszcze" "polska"))
(check-expect(remove-articles sample2)
             (list(list "jeszcze" "polska")(list "nie" "zginela")))


;;(write-file "noart.dat"(collapse(remove-articles (read-words/line "ttt.txt"))))

;; ----------------------------------------------
;; Exercise 174

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))


(define(encode-word word)
  (cond
    ((empty? word)'())
    (else(cons(code1(first word))
              (encode-word(rest word))))))


(define(implode2 strings)
  (cond
    ((empty? strings)"")
    (else(string-append(first strings)
                       (implode2(rest strings))))))


(check-expect(encode-word(list "a" "b" "c"))(cons "97" (cons "98" (cons "99" '()))))

(define line45(list "abc" "def"))
(define line46(list "ghi" "jkf"))
(define text523(cons line45(cons line46 '())))

(define(encode-line line)
  (cond
    ((empty? line)'())
    (else(cons(implode2(encode-word(explode(first line))))
              (encode-line(rest line))))))

(check-expect(encode-line line45)(cons "979899" (cons "100101102" '())))
(check-expect(encode-line line46)(cons "103104105" (cons "106107102" '())))

(define(encode-all sometext)
  (cond
    ((empty? sometext)'())
    (else(cons(encode-line(first sometext))
              (encode-all(rest sometext))))))

(check-expect
 (encode-all text523)
 (cons(cons "979899" (cons "100101102" '()))
      (cons(cons "103104105" (cons "106107102" '()))'())))

;;(encode-all(read-words/line "ttt.txt"))

;; Exercise 175
(define(count-lines sometext)
  (length sometext))


(check-expect(count-lines text523)2)
(check-expect(count-lines lls0)0)


(define(count-words sometext)
  (cond
    ((empty? sometext)0)
    (else(+(length(first sometext))
           (count-words(rest sometext))))))

(check-expect(count-words text523)4)
(check-expect(count-words lls0)0)

(define(count-letters-line line)
  (cond
    ((empty? line)0)
    (else(+(string-length(first line))
           (count-letters-line(rest line))))))


(check-expect(count-letters-line line45)6)


(define(count-letters sometext)
  (cond
    ((empty? sometext)0)
    (else(+(count-letters-line(first sometext))
           (count-letters(rest sometext))))))


(check-expect(count-letters text523)12)

;; returns number of letters,words and lines
(define(wc filename)
  (cons(count-letters(read-words/line filename))
       (cons(count-words(read-words/line filename))
            (cons(count-lines(read-words/line filename))'()))))

;; returns number of letters,words and lines
(define(wc-text sometext)
  (cons(count-letters sometext)
       (cons(count-words sometext)
            (cons(count-lines sometext)'()))))
  

(check-expect(wc-text text523)(cons 12(cons 4(cons 2 '()))))

;;(ws "ttt.txt")

;;---------------------------------------------------
;; MATRIX


;; examples
;;------------------------------------------------
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))


(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
;;------------------------------------------------

;; helpers
(define(first* lln)
  (cond
    ((empty? lln)'())
    (else(cons(first(first lln))
              (first*(rest lln))))))

(define(rest* lln)
  (cond
    ((empty? lln)'())
    (else(cons(rest(first lln))
              (rest*(rest lln))))))


(check-expect(first* mat1)(cons 11(cons 21 '())))
(check-expect(rest* mat1)(cons (cons 12 '()) (cons (cons 22 '()) '())))


;; examples
;;------------------------------------------------
(define bigrow1(cons 1(cons 2(cons 3 '()))))
(define bigrow2(cons 4(cons 5(cons 6 '()))))
(define bigmat(cons bigrow1(cons bigrow2 '())))


(define bigrow3(cons 1(cons 4 '())))
(define bigrow4(cons 2(cons 5 '())))
(define bigrow6(cons 3(cons 6 '())))
(define bigmat2(cons bigrow3(cons bigrow4(cons bigrow6 '()))))
;;------------------------------------------------


; Matrix -> Matrix
; transposes the given matrix along the diagonal 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

(check-expect (transpose mat1) tam1)
(check-expect (transpose bigmat) bigmat2)