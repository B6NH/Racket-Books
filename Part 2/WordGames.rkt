;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname WordGames) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
 

(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)




(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))


 
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
(define (in-dictionary los)
  (cond
    ((empty? los)'())
    (else(if(member?(car los)AS-LIST)
            (cons(car los)(in-dictionary(rest los)))
            (in-dictionary(rest los))))))
     
    

; String -> Word
; converts s to the chosen word representation 
(define (string->word s) (explode s))

; Word -> String
; converts w to a string
(define (word->string w) (implode w))


; List-of-words -> List-of-strings
; turns all Words in low into Strings
(define(words->strings ws)
  (cond
    ((empty? ws)'())
    (else(cons(word->string(car ws))
              (words->strings(rest ws))))))


;;------------------------------------------------------------------
;;------------------------------------------------------------------
;; Solution from twfarland github


; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))


(define (insert-everywhere/in-all-words l alow)
  (cond
    [(empty? alow) empty]
    [else (append (insert-everywhere l (first alow))
                  (insert-everywhere/in-all-words l (rest alow)))]))

(define (insert-everywhere l a-word)
  (cond
    [(empty? a-word) (list(list l))]
    [else
     (cons (cons l a-word)
           (prefix-all (first a-word)
                       (insert-everywhere l (rest a-word))))]))


(define (prefix-all l words) 
  (cond
    [(empty? words) empty]
    [else (cons (cons l (first words))
                (prefix-all l (rest words)))])) 


;;(insert-everywhere "z" (list "a" "b" "c"))