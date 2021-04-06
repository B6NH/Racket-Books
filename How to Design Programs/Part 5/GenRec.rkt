;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname GenRec) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; V Generative Recursion


; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (map implode(list->chunks s n)))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))


(check-expect
 (bundle(explode "abcdefg")3)
 (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())

;; Exercise 422. Define the function list->chunks.

(check-expect
 (list->chunks'("asd" 12 "b" 55 #true #false 2)2)
 '(("asd" 12)("b" 55)(#true #false)(2)))

;; List Number -> List-of-Lists
(define(list->chunks l n)
  (cond
    [(empty? l)'()]
    [else(cons(take l n)(list->chunks(drop l n)n))]))


;; Exercise 423. Define partition. It consumes a String s and a natural number n.

(check-expect
 (partition "something" 3)
 '("som" "eth" "ing"))
(check-expect
 (partition "something" 3)
 (bundle(explode "something")3))

(check-expect
 (partition "something" 4)
 '("some" "thin" "g"))
(check-expect
 (partition "something" 4)
 (bundle(explode "something")4))

(check-expect
 (partition "" 4)
 '())
(check-expect
 (partition "" 4)
 (bundle(explode "")4))

;; String Number -> List-of-Strings
(define(partition s n)
  (local((define length(string-length s)))
    (cond
      ((zero? length)'())
      ((> n length)(list s))
      (else(cons (substring s 0 n)
                 (partition(substring s n)n))))))


;; Exercise 424. Draw a quick-sort diagram like the one

(check-expect
 (smallers 5 '(1 2 3 4 5 6 7 8))
 '(1 2 3 4))
(check-expect
 (largers 5 '(1 2 3 4 5 6 7 8))
 '(5 6 7 8))

(check-expect
 (sort<'())
 '())
(check-expect
 (sort<'(7 9 2 3))
 '(2 3 7 9))
(check-expect
 (sort<'(11 9 2 18 12 14 18 2 4 1))
 '(1 2 2 4 9 11 12 14 18 18))


(check-expect
 (quick-sort<'())
 '())
(check-expect
 (quick-sort<'(11 8 14 7))
 '(7 8 11 14))
(check-expect
 (quick-sort<'(11 9 2 18 12 14 4 1))
 '(1 2 4 9 11 12 14 18))
(check-expect
 (quick-sort<'(3 2 2 1))
 '(1 2 2 3))
(check-expect
 (quick-sort<'(11 9 2 18 12 14 18 2 4 1))
 '(1 2 2 4 9 11 12 14 18 18))


(check-expect
 (quick-sort<-v2'())
 '())
(check-expect
 (quick-sort<-v2'(7 12 3 1))
 '(1 3 7 12))
(check-expect
 (quick-sort<-v2'(11 9 2 18 12 14 4 1))
 '(1 2 4 9 11 12 14 18))
(check-expect
 (quick-sort<-v2'(7 7 3 1))
 '(1 3 7 7))
(check-expect
 (quick-sort<-v2'(11 9 2 18 12 14 18 2 4 1))
 '(1 2 2 4 9 11 12 14 18 18))


(check-expect
 (quick-sort<-v3'())
 '())
(check-expect
 (quick-sort<-v3'(7 12 3 1))
 '(1 3 7 12))
(check-expect
 (quick-sort<-v3'(11 9 2 18 12 14 4 1))
 '(1 2 4 9 11 12 14 18))
(check-expect
 (quick-sort<-v3'(7 7 3 1))
 '(1 3 7 7))
(check-expect
 (quick-sort<-v3'(11 9 2 18 12 14 18 2 4 1))
 '(1 2 2 4 9 11 12 14 18 18))


(check-expect
 (quick-sort-abstract'() <)
 '())
(check-expect
 (quick-sort-abstract'(7 12 3 1)<)
 '(1 3 7 12))
(check-expect
 (quick-sort-abstract'(11 9 2 18 12 14 4 1)<)
 '(1 2 4 9 11 12 14 18))
(check-expect
 (quick-sort-abstract'(7 7 3 1)<)
 '(1 3 7 7))
(check-expect
 (quick-sort-abstract'(11 9 2 18 12 14 18 2 4 1)<)
 '(1 2 2 4 9 11 12 14 18 18))

(check-expect
 (quick-sort-abstract'() >)
 '())
(check-expect
 (quick-sort-abstract'(7 12 3 1)>)
 '(12 7 3 1))
(check-expect
 (quick-sort-abstract'(11 9 2 18 12 14 4 1)>)
 '(18 14 12 11 9 4 2 1))
(check-expect
 (quick-sort-abstract'(7 7 3 1)>)
 '(7 7 3 1))
(check-expect
 (quick-sort-abstract'(11 9 2 18 12 14 18 2 4 1)>)
 '(18 18 14 12 11 9 4 2 2 1))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))


; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty?(rest alon))(list(first alon))]
    [else
     (local((define pivot(first alon))(define rst(rest alon)))
       (append(quick-sort<(smallers pivot rst))
              (list pivot)
              (quick-sort<(largers pivot rst))))]))

;; for large lists use quicksort
;; for small lists use sort with insert
(define(quick-sort<-v2 alon)
  (if(>(length alon)5)
     (quick-sort< alon)
     (sort< alon)))

(define(largers pivot alon)
  (filter(lambda(x)(>= x pivot))alon))

(define(smallers pivot alon)
  (filter(lambda(x)(< x pivot))alon))

;; Exercise 430. Develop a variant of quick-sort< that uses only

(define (quick-sort<-v3 alon)
  (cond
    [(empty? alon) '()]
    [(empty?(rest alon))(list(first alon))]
    [else
     (local((define pivot(first alon))
            (define rst(rest alon))
            (define passed(filter(lambda(x)(< x pivot))rst))
            (define not-passed(filter(lambda(x)(not(member? x passed)))rst)))
       (append(quick-sort<-v3 passed)
              (list pivot)
              (quick-sort<-v3 not-passed)))]))


(define (quick-sort-abstract alon comp)
  (cond
    [(empty? alon) '()]
    [(empty?(rest alon))(list(first alon))]
    [else
     (local((define pivot(first alon))
            (define rst(rest alon))
            (define passed(filter(lambda(x)(comp x pivot))rst))
            (define not-passed(filter(lambda(x)(not(member? x passed)))rst)))
       (append(quick-sort-abstract passed comp)
              (list pivot)
              (quick-sort-abstract not-passed comp)))]))


;; Exercise 433. Develop a checked version of bundle

(define (checked-bundle s n)
  (if(and(not(empty? s))(<= n 0))
     (error "wrong n argument")
     (map implode(list->chunks s n))))

(check-expect
 (checked-bundle(explode "abcdefg")3)
 (list "abc" "def" "g"))
(check-expect (checked-bundle '("a" "b") 3) (list "ab"))
(check-expect (checked-bundle '() 3) '())
(check-expect (checked-bundle '() 0) '())
(check-expect (checked-bundle '() -5) '())
(check-error (checked-bundle '("a" "f" "c")0)"wrong n argument")
(check-error (checked-bundle '("a" "f" "c")-4)"wrong n argument")


;; Exercise 437. Define solve and combine-solutions

(define(special input)
  (foldr(lambda(x y)(+ 1 y))0 input))

(check-expect(special'(5 2 3))3)
(check-expect(special'())0)

(define(special2 input)
  (foldr(lambda(x y)(cons(* -1 x) y))'()input))

(check-expect(special2'(5 -2 3))'(-5 2 -3))
(check-expect(special2'())'())

(define(special3 input)
  (foldr(lambda(x y)(cons(string-upcase x)y))'()input))

(check-expect
 (special3'("aGe" "seX" "location" "WAVE"))
 '("AGE" "SEX" "LOCATION" "WAVE"))

;; Completing the first three steps of the design recipe is straightforward:

; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)

(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))


;; In short, the answers for the four design-recipe questions fall out.

(check-expect (gcd-generative 6 25) 1)
(check-expect (gcd-generative 18 24) 6)

(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S))
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

;; Exercise 440. Copy gcd-generative

;; (time (gcd-generative 101135853 45014640))
;; (time (gcd-structural 101135853 45014640))

;; Exercise 442. Add sort< and quick-sort< to the definitions area.

(define(create-test size)
  (cond
    ((zero? size)'())
    (else(cons(random 100)(create-test(sub1 size))))))

;; cross-over point is 10
(define small-size 10)

(define unsorted1(create-test 1000))
(define unsorted2(create-test small-size))
(define unsorted3(reverse(build-list small-size(lambda(x)x))))
(define unsorted4(build-list small-size(lambda(x)x)))

;; for large list quick-sort is faster
(define sorted1(time(sort< unsorted1)))
(define sorted2(time(quick-sort< unsorted1)))

(define sorted3(time(sort< unsorted2)))
(define sorted4(time(quick-sort< unsorted2)))

(define sorted5(time(sort< unsorted3)))
(define sorted6(time(quick-sort< unsorted3)))

(define sorted7(time(sort< unsorted4)))
(define sorted8(time(quick-sort< unsorted4)))


;; cross-over point is 10
(define(clever-sort alon)
  (cond
    ((empty? alon)'())
    ((empty?(rest alon))(list(first alon)))
    ((<(length alon)10)(insert(first alon)(clever-sort(rest alon))))
    (else
     (local((define pivot(first alon))(define rst(rest alon)))
       (append(clever-sort(smallers pivot rst))
              (list pivot)
              (clever-sort(largers pivot rst)))))))


(check-expect
 (clever-sort'())
 '())
(check-expect
 (clever-sort'(7 12 3 1))
 '(1 3 7 12))
(check-expect
 (clever-sort'(11 9 2 18 12 14 4 1))
 '(1 2 4 9 11 12 14 18))
(check-expect
 (clever-sort'(7 7 3 1))
 '(1 3 7 7))
(check-expect
 (clever-sort'(11 9 2 18 12 14 18 2 4 1))
 '(1 2 2 4 9 11 12 14 18 18))
(check-expect
 (clever-sort unsorted3)
 (reverse unsorted3))
(check-expect
 (clever-sort unsorted4)
 unsorted4)

;; Exercise 444. Exercise 443 means that the design for gcd-structural

(check-expect
 (largest-common'(6 2 3 4 9)'(2 3 9 2 1))
 9)

(check-error
 (largest-common'()'(2 3 9 2 1)))

(check-expect
 (all-common'(6 1 3 8 2)'(2 6 7 3))
 '(6 3 2))
(check-expect
 (all-common'()'(2 6 7 3))
 '())
(check-expect
 (all-common'(6 1 3 8 2)'())
 '())
(check-expect
 (all-common'()'())
 '())

(check-expect
 (divisors 18 18)
 '(18 9 6 3 2 1))

(define(divisors k l)
  (cond
    ((= 1 k)(list k))
    ((zero?(remainder l k))
     (cons k(divisors(sub1 k)l)))
    (else(divisors(sub1 k)l))))

(define(all-common a b)
  (foldr(lambda(x y)
          (if(member? x b)(cons x y)y))'()a))

(define(largest alon)
  (if(empty? alon)
     (error "cant find max of empty list")
     (local
       ((define(h current-max alon)
          (cond
            ((empty? alon)current-max)
            (else
             (local((define frst(first alon))(define rst(rest alon)))
               (h (if(> frst current-max)frst current-max) rst))))))
       (h(first alon)alon))))

(check-expect
 (largest'(1 4 29 2 3 4))
 29)
(check-error
 (largest '())
 "cant find max of empty list")

(define(largest-common a b)
  (largest(all-common a b)))

(define (gcd-structural-v2 S L)
  (largest-common (divisors S S) (divisors S L)))

(check-expect (gcd-structural-v2 6 25) 1)
(check-expect (gcd-structural-v2 18 24) 6)
