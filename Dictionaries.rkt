;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Dictionaries) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))


(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))


;; word start with this letter?
(define(word-starts-with letter word)
  (string=?(first(explode word))
           letter))

;; how many words start with letter
(define(starts-with# letter dict)
  (cond
    ((empty? dict)0)
    (else(+(if(word-starts-with letter(first dict))
              1 0)
              (starts-with# letter(rest dict))))))


(define testdict1 (list "daniel" "computer" "amen" "cat" "ares"))
(check-expect(starts-with# "a" testdict1)2)
(check-expect(starts-with# "d" testdict1)1)
(check-expect(starts-with# "r" testdict1)0)


(define-struct Letter-Counts[letter count])


(define(count-by-letter-helper LETTERS dict)
  (cond
    ((empty? LETTERS)'())
    (else(cons(make-Letter-Counts(first LETTERS)
                                 (starts-with#(first LETTERS)dict))
              (count-by-letter-helper(rest LETTERS)dict)))))

;; list of Letter-Counts structures for every letter
(define(count-by-letter dict)
  (count-by-letter-helper LETTERS dict))

(define letcounts(list
              (make-Letter-Counts "a" 2)
              (make-Letter-Counts "b" 0)
              (make-Letter-Counts "c" 2)
              (make-Letter-Counts "d" 1)
              (make-Letter-Counts "e" 0)
              (make-Letter-Counts "f" 0)
              (make-Letter-Counts "g" 0)
              (make-Letter-Counts "h" 0)
              (make-Letter-Counts "i" 0)
              (make-Letter-Counts "j" 0)
              (make-Letter-Counts "k" 0)
              (make-Letter-Counts "l" 0)
              (make-Letter-Counts "m" 0)
              (make-Letter-Counts "n" 0)
              (make-Letter-Counts "o" 0)
              (make-Letter-Counts "p" 0)
              (make-Letter-Counts "q" 0)
              (make-Letter-Counts "r" 0)
              (make-Letter-Counts "s" 0)
              (make-Letter-Counts "t" 0)
              (make-Letter-Counts "u" 0)
              (make-Letter-Counts "v" 0)
              (make-Letter-Counts "w" 0)
              (make-Letter-Counts "x" 0)
              (make-Letter-Counts "y" 0)
              (make-Letter-Counts "z" 0)))

(check-expect(count-by-letter testdict1)
             letcounts)


(define(letterc> one two)
  (>(Letter-Counts-count one)
    (Letter-Counts-count two)))

(define(max-letter-counts-helper lettercounts currentmax)
  (cond
    ((empty? lettercounts)currentmax)
    (else(if(letterc>(first lettercounts)currentmax)
            (max-letter-counts-helper(rest lettercounts)(first lettercounts))
            (max-letter-counts-helper(rest lettercounts)currentmax)))))

;; finds structure for most popular letter in list
(define(max-letter-counts lettercounts)
  (max-letter-counts-helper lettercounts (make-Letter-Counts "a" 0)))

(check-expect(max-letter-counts letcounts)
             (make-Letter-Counts "a" 2))

;; structure for most popular letter in dict
(define(most-frequent dict)
  (max-letter-counts(count-by-letter dict)))


(check-expect(most-frequent testdict1)
             (make-Letter-Counts "a" 2))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (letterc> n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

;; find Letter-Counts structure with largest count
(define(most-frequent2 dict)
  (first(sort>(count-by-letter dict))))

(check-expect(most-frequent2 testdict1)
             (make-Letter-Counts "c" 2))


(define testdict2(list "elephant" "fire" "dog" "dream" "dear"))

(check-expect(most-frequent testdict2)
             (make-Letter-Counts "d" 3))

(check-expect(most-frequent2 testdict2)
             (make-Letter-Counts "d" 3))


;; ------------------------------------------------------------

;; dictionary for one letter
(define(produce-dict-for-letter letter dict)
  (cond
    ((empty? dict)'())
    (else(if(word-starts-with letter(car dict))
            (cons(car dict)(produce-dict-for-letter letter(rest dict)))
            (produce-dict-for-letter letter(rest dict))))))

(check-expect(produce-dict-for-letter "x" AS-LIST)
             (list
              "x"
              "xenon"
              "xenon's"
              "xenophobia"
              "xenophobia's"
              "xenophobic"
              "xerographic"
              "xerography"
              "xerography's"
              "xylem"
              "xylem's"
              "xylophone"
              "xylophone's"
              "xylophones"
              "xylophonist"
              "xylophonist's"
              "xylophonists"))


(define(words-by-first-letter-helper dict LETTERS)
  (cond
    ((empty? LETTERS)'())
    (else(cons(produce-dict-for-letter (car LETTERS) AS-LIST)
              (words-by-first-letter-helper dict(rest LETTERS))))))

;; list of dictionaries for every letter
(define(words-by-first-letter dict)
  (words-by-first-letter-helper dict LETTERS))




(define(largest-dictionary-helper lst currentmax)
  (cond
    ((empty? lst)currentmax)
    (else(if(>(length(car lst))
              (length currentmax))
            (largest-dictionary-helper(rest lst)(car lst))
            (largest-dictionary-helper(rest lst)currentmax)))))


;; finds largest dictionary
(define(largest-dictionary lst)
  (largest-dictionary-helper lst (car lst)))
            

(define(most-frequent3 dict)
  (make-Letter-Counts
   (first(explode(first(largest-dictionary(words-by-first-letter dict)))))
   (length(largest-dictionary(words-by-first-letter dict)))))


(check-expect(most-frequent3 AS-LIST)
             (most-frequent2 AS-LIST))


(check-expect(most-frequent AS-LIST)
             (most-frequent3 AS-LIST))




