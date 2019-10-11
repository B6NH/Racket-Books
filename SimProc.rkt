;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname SimProc) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; 23 Simultaneous Processing

;; Case 1

(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
(check-expect(replace-eol-with '(1 2 3)'())'(1 2 3))


(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))

(check-expect(cross'(a b c)'(5 6))
             '((a 5)(a 6)(b 5)(b 6)(c 5)(c 6)))
(check-expect
 (combinations 'a '(4 5 6))
 '((a 4)(a 5)(a 6)))
             

(define(combinations symbol numbers)
  (cond
    ((empty? numbers)'())
    (else
     (cons(cons symbol(cons(first numbers)'()))
          (combinations symbol(rest numbers))))))

(define(cross symbols numbers)
  (cond
    ((empty? symbols)'())
    (else(append(combinations(first symbols)numbers)
                (cross(rest symbols)numbers)))))

;; Case 2

; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length 
(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons
       (weekly-wage (first hours) (first wages/h))
       (wages*.v2 (rest hours) (rest wages/h)))]))

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

;; Exercise 388. In the real world

(define-struct employee [name number payrate])
(define-struct workrecord [employeename hours])
(define-struct weeklywage [employeename wage])

(define employees
  (list(make-employee "magda" 1234 3)
       (make-employee "ewa" 4321 2)
       (make-employee "justyna" 3322 4)
       (make-employee "agnieszka" 2233 2)
       (make-employee "lena" 4567 3)))

(define workrecords
  (list(make-workrecord "lena" 10)
       (make-workrecord "agnieszka" 12)
       (make-workrecord "justyna" 13)
       (make-workrecord "ewa" 10)
       (make-workrecord "magda" 11)))

(check-expect
 (wages employees workrecords)
 (list(make-weeklywage "magda" 33)
      (make-weeklywage "ewa" 20)
      (make-weeklywage "justyna" 52)
      (make-weeklywage "agnieszka" 24)
      (make-weeklywage "lena" 30)))

(check-expect
 (weekly-wage-for(make-employee "ewa" 4321 2)workrecords)
 (make-weeklywage "ewa" 20))

(check-error(weekly-wage-for(make-employee "natalia" 9988 2)workrecords))

(define(weekly-wage-for employee work-records)
  (if(empty? work-records)
     (error "no workecords for this employee")
     (local((define name(employee-name employee)))
       (if(string=? name
                    (workrecord-employeename(first work-records)))
          (make-weeklywage name
                           (*(employee-payrate employee)
                             (workrecord-hours(first work-records))))
          (weekly-wage-for employee(rest work-records))))))

(define (wages employees-list work-records)
  (if(empty? employees-list)
     '()
     (cons
      (weekly-wage-for (first employees-list) work-records)
      (wages (rest employees-list) work-records))))

;; Exercise 389. Design the zip function

(check-expect
 (zip '("ania" "magda" "ewa" "julia")
      '("123" "456" "789" "987"))
 (list(make-phone-record "ania" "123")
      (make-phone-record "magda" "456")
      (make-phone-record "ewa" "789")
      (make-phone-record "julia" "987")))

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

(define(zip names phones)
  (cond
    ((empty? names)'())
    (else(cons(make-phone-record(first names)(first phones))
              (zip(rest names)(rest phones))))))


;; Case 3

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list-pick: list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list-pick: list too short")

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
(define (list-pick l n)
  (cond
    [(empty? l)(error 'list-pick "list too short")]
    [(= n 0)(first l)]
    [(> n 0) (list-pick (rest l) (sub1 n))]))


;; Exercise 390. Design the function tree-pick.

; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

(define-struct branch [left right])

(define tree
  (make-branch
   (make-branch 'a 'b)
   (make-branch
    (make-branch 'c 'd)
    (make-branch 'e 'f))))

(check-expect(tree-pick tree'(left left))'a)
(check-expect(tree-pick tree'(left right))'b)
(check-expect(tree-pick tree'(right left left))'c)
(check-expect(tree-pick tree'(right left right))'d)
(check-error(tree-pick tree'(left left left left)))
(check-error(tree-pick tree'(right right)))

(define(tree-pick tree path)
  (cond
    ((empty? path)(if(branch? tree)(error "symbol not found")tree))
    ((symbol? tree)(error "symbol found too early"))
    (else
     (local((define direction(first path)))
       (tree-pick
        (if(symbol=? direction 'left)
           (branch-left tree)(branch-right tree))
        (rest path))))))


;; Exercise 391. Design replace-eol-with

(check-expect (replace-eol-with2 '() '(a b)) '(a b))
(check-expect (replace-eol-with2 (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with2
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
(check-expect(replace-eol-with2 '(1 2 3)'())'(1 2 3))

(define(replace-eol-with2 front end)
  (cond
    ((empty? front)end)
    (else
     (cons(first front)
          (replace-eol-with2(rest front)end)))))


;; Exercise 393. Figure 62 presents two data

(check-expect(union'(1 2 3)'(4 5 6))'(1 2 3 4 5 6))
(check-expect(union'(1 2 3 4)'(4 5 6))'(1 2 3 4 5 6))
(check-expect(union'(0 1 2)'(2 3 4 5 6))'(0 1 2 3 4 5 6))
(check-expect(intersect'(1 2 3 4)'(3 4 5))'(3 4))
(check-expect(intersect'()'(3 4 5))'())
(check-expect(intersect'(3 4 5)'())'())

(define(union one two)
  (cond
    ((empty? one)two)
    (else
     (if(member?(first one)two)
        (union(rest one)two)
        (cons(first one)(union(rest one)two))))))

(define(intersect one two)
  (cond
    ((empty? one)'())
    (else
     (if(member?(first one)two)
        (cons(first one)(intersect(rest one)two))
        (intersect(rest one)two)))))


;; Exercise 394. Design merge.

(check-expect
 (merge'(1 4 6 6 8)
       '(2 3 5 6 7))
 '(1 2 3 4 5 6 6 6 7 8))
(check-expect
 (merge'(12 13 14 15)
       '(1 2 3 4))
 '(1 2 3 4 12 13 14 15))
(check-expect
 (merge'(1 2 3 4)
       '(12 13 14 15))
 '(1 2 3 4 12 13 14 15))
(check-expect
 (merge'(1 1 9)'(1 2 3 4 5))
 '(1 1 1 2 3 4 5 9))

(define(merge one two)
  (cond
    ((empty? one)two)
    ((empty? two)one)
    (else
     (local((define firstone(first one))
            (define firsttwo(first two)))
       (if(< firstone firsttwo)
          (cons firstone(merge(rest one)two))
          (cons firsttwo(merge one(rest two))))))))


;; Exercise 395. Design take.

(check-expect
 (take'(6 2 3 4 5 1 2)3)
 '(6 2 3))
(check-expect
 (take'(6 2 3 4 5 1 2)0)
 '())
(check-expect
 (take'(6 2 3 4 5 1 2)20)
 '(6 2 3 4 5 1 2))
(check-expect
 (take'()3)'())

(define(take li nu)
  (cond
    ((and(cons? li)(> nu 0))
     (cons(first li)(take(rest li)(- nu 1))))
    (else '())))

(check-expect
 (drop'(6 2 3 4 5 1 2)3)
 '(4 5 1 2))
(check-expect
 (drop'(6 2 3 4 5 1 2)0)
 '(6 2 3 4 5 1 2))
(check-expect
 (drop'(6 2 3 4 5 1 2)20)
 '())
(check-expect
 (drop'()3)'())

(define(drop li nu)
  (cond
    ((and(cons? li)(> nu 0))
     (drop(rest li)(- nu 1)))
    (else li)))


;; Exercise 396. Hangman is a well-known

(define LETTERS
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" 
        "j" "k" "l" "m" "n" "o" "p" "q"
        "r" "s" "t" "u" "v" "w" "x" "y" "z"))



; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 

; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

(define(compare-word answer current letter)
  (if(empty? answer)'()
     (local((define first-current(first current)))
       (cons(if(and(string=? first-current "_")
                   (string=?(first answer)letter))letter first-current)
            (compare-word(rest answer)(rest current)letter)))))

(check-expect
 (compare-word'("k" "o" "s" "z" "t")
              '("_" "_" "_" "_" "_")
              "k")
 '("k" "_" "_" "_" "_"))
(check-expect
 (compare-word'("k" "o" "s" "z" "t")
              '("k" "_" "_" "_" "_")
              "t")
 '("k" "_" "_" "_" "t"))
(check-expect
 (compare-word'("k" "o" "s" "z" "t")
              '("k" "_" "_" "_" "t")
              "r")
 '("k" "_" "_" "_" "t"))


;; Exercise 397. In a factory, employees punch time cards

(define-struct punch-record [number hours])
(define-struct employee-record [name number rate])
(define-struct wage-record [name wage])


(define employee-records
  (list(make-employee-record "Michael" 1234 5)
       (make-employee-record "Lena" 1122 6)
       (make-employee-record "Eva" 3412 7)))
(define punch-card-records
  (list(make-punch-record 1122 10)
       (make-punch-record 3412 9)
       (make-punch-record 1234 11)))
       

(check-expect
 (wages*.v3 employee-records punch-card-records)
 (list(make-wage-record "Michael"(* 5 11))
      (make-wage-record "Lena"(* 6 10))
      (make-wage-record "Eva"(* 7 9))))
;; cant find card for employee
(check-error
 (wages*.v3 employee-records
            (list(make-punch-record 1122 10)
                 (make-punch-record 3412 9))))
;; cant find employee for card
(check-error
 (wages*.v3 employee-records
            (list(make-punch-record 1122 10)
                 (make-punch-record 3412 9)
                 (make-punch-record 1234 11)
                 (make-punch-record 7234 8))))
(check-expect
 (wage-for(make-employee-record "Eva" 3412 7)
          punch-card-records)
 (make-wage-record "Eva"(* 7 9)))
(check-error
 (wage-for(make-employee-record "Eva" 8231 7)
          punch-card-records))

(define(wage-for em-rec pun-recs)
  (cond
    ((empty? pun-recs)(error "WRONG"))
    (else
     (local((define current-pun(first pun-recs)))
       (if(=(employee-record-number em-rec)
            (punch-record-number current-pun))
          (make-wage-record
           (employee-record-name em-rec)
           (*(employee-record-rate em-rec)
             (punch-record-hours current-pun)))
          (wage-for em-rec(rest pun-recs)))))))


(define(wages*.v3 em-recs pun-recs)
  (if(not(=(length em-recs)(length pun-recs)))
     (error "WRONG")
     (local
       ((define(helper em-recs pun-recs)
          (cond
            ((empty? em-recs)'())
            (else
             (local((define em-rec(first em-recs)))
               (cons
                (wage-for em-rec pun-recs)
                (helper(rest em-recs)pun-recs)))))))
       (helper em-recs pun-recs))))


;; Exercise 398. A linear combination is the sum

(define example-linear-combination
  (list 5 17 3))
(define example-variable-values
  (list 10 1 2))

(check-expect
 (value example-linear-combination example-variable-values)
 (+(* 5 10)(* 17 1)(* 3 2)))

(define(value linear-comb variable-vals)
  (cond
    ((empty? linear-comb)0)
    (else
     (+(*(first linear-comb)
         (first variable-vals))
       (value(rest linear-comb)
             (rest variable-vals))))))

;; Exercise 399. Louise, Jane, Laura, Dana, and Mary

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))

(define (prefix-all l words) 
  (cond
    [(empty? words) empty]
    [else (cons (cons l (first words))
                (prefix-all l (rest words)))]))

(define (insert-everywhere l a-word)
  (cond
    [(empty? a-word) (list(list l))]
    [else
     (cons (cons l a-word)
           (prefix-all (first a-word)
                       (insert-everywhere l (rest a-word))))]))

(define (insert-everywhere/in-all-words l alow)
  (cond
    [(empty? alow) empty]
    [else (append (insert-everywhere l (first alow))
                  (insert-everywhere/in-all-words l (rest alow)))]))
 
; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
; see exercise 213
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (local
    ((define size(length l))
     (define random-index(random size)))
    (list-pick l random-index)))

(check-expect
 (arrangements'("a" "b" "c"))
 (list
  (list "a" "b" "c")
  (list "b" "a" "c")
  (list "b" "c" "a")
  (list "a" "c" "b")
  (list "c" "a" "b")
  (list "c" "b" "a")))
(check-expect
 (non-same-one'("Louise" "Jane" "Laura")
              '("Jane" "Laura" "Louise"))
 #true)
(check-expect
 (non-same-one'("Louise" "Jane" "Laura")
              '("Laura" "Louise" "Jane"))
 #true)
(check-expect
 (non-same-one'("Louise" "Jane" "Laura")
              '("Laura" "Jane" "Louise"))
 #false)
              

;; check if 2 lists dont agree in any place
(define(non-same-one names names2)
  (cond
    ((empty? names)#true)
    (else
     (and(not(string=?(first names)(first names2)))
         (non-same-one(rest names)(rest names2))))))


; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place 
(define (non-same names ll)
  (cond
    ((empty? ll)'())
    (else
     (if(non-same-one names(first ll))
        (cons(first ll)(non-same names(rest ll)))
        (non-same names(rest ll))))))

(check-expect
 (non-same
  '("Louise" "Jane" "Laura")
  '(("Louise" "Jane" "Laura")
    ("Louise" "Laura" "Jane")
    ("Jane" "Louise" "Laura")
    ("Jane" "Laura" "Louise")
    ("Laura" "Louise" "Jane")
    ("Laura" "Jane" "Louise")))
 '(("Jane" "Laura" "Louise")
   ("Laura" "Louise" "Jane")))


(define all-names
  '("Louise" "Jane" "Laura" "Dana" "Mary"))

;; check if answer doesnt agree with all-names in any place
(define(non-same-with-all-names answer)
  (non-same-one all-names answer))

(check-satisfied
 (gift-pick all-names)non-same-with-all-names)

;; Exercise 400. Design the function DNAprefix.

(check-expect
 (DNAprefix
  '(a c t g c)
  '(a c t g c g a c t))
 #true)

(check-expect
 (DNAprefix
  '(a c t g c)
  '(a g t g c g t c g))
 #false)


(define(DNAprefix pattern search-string)
  (cond
    ((empty? pattern)#true)
    (else
     (and(symbol=?(first pattern)
                  (first search-string))
         (DNAprefix(rest pattern)
                   (rest search-string))))))


(check-expect
 (DNAdelta
  '(a t c g a)
  '(a t c g a t g c))
 't)

(check-error
 (DNAdelta
  '(a t c g a)
  '(a t c g a)))

(check-expect
 (DNAdelta
  '(a t c g a)
  '(a t a g a t g c))
 #false)

(define(DNAdelta pattern search-string)
  (cond
    ((empty? pattern)
     (if(empty? search-string)
        (error "WRONG")
        (first search-string)))
    (else
     (if(symbol=?(first pattern)
                 (first search-string))
        (DNAdelta(rest pattern)
                 (rest search-string))
        #false))))
          

;; Exercise 401. Design sexp=?, a function that determines

(check-expect
 (sexp=? 5 5)#true)
(check-expect
 (sexp=? 5 "5")#false)
(check-expect
 (sexp=? "abc" "abc")#true)
(check-expect
 (sexp=? 'a 'a)#true)

(check-expect
 (sexp=? '() '())
 #true)
(check-expect
 (sexp=? '("g") '("g"))
 #true)
(check-expect
 (sexp=?'(1("a" 'z)3)'(1("a" 'z)3))
 #true)
(check-expect
 (sexp=?'(1("a" 'z 3)3)'(1("a" 'z)3))
 #false)
(check-expect
 (sexp=?'(1("a"(12 'z)3))
        '(1("a"(12 'm)3)))
 #false)

(define(sexp=? one two)
  (cond
    ((and(string? one)(string? two))(string=? one two))
    ((and(number? one)(number? two))(= one two))
    ((and(symbol? one)(symbol? two))(symbol=? one two))
    (else
     (or(and(empty? one)(empty? two))
        (and(cons? one)(cons? two)
            (sexp=?(first one)(first two))
            (sexp=?(rest one)(rest two)))))))

