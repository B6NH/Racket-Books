;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname InterDa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; IV Intertwined Data, htdp second edition


(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))

; An FT is one of: 
; – NP
; – (make-child FT FT String N String)


; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))


 
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))


;; ------------------------------------------------------------------
;; ------------------------------------------------------------------

(define(count-persons an-ftree)
  (cond
    ((no-parent? an-ftree)0)
    (else(+ 1
            (count-persons(child-father an-ftree))
            (count-persons(child-mother an-ftree))))))

(define(sum-ages an-ftree current-year)
  (cond
    ((no-parent? an-ftree)0)
    (else(+ (- current-year(child-date an-ftree))
            (sum-ages(child-father an-ftree)current-year)
            (sum-ages(child-mother an-ftree)current-year)))))
  

(define(average-age an-ftree current-year)
  (/(sum-ages an-ftree current-year)(count-persons an-ftree)))


(define(eye-colors an-ftree)
  (cond
    ((no-parent? an-ftree)'())
    (else(append(list(child-eyes an-ftree))
                (eye-colors(child-father an-ftree))
                (eye-colors(child-mother an-ftree))))))


(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
(check-expect(count-persons Gustav)5)
(check-expect(count-persons Carl)1)
(check-expect(sum-ages Adam 2019)255)
(check-expect(sum-ages Bettina 2019)93)
(check-expect(average-age Adam 2019)85)
(check-expect
 (eye-colors Gustav)
 (list "brown" "pink" "blue" "green" "green"))

(check-expect (blue-eyed-child? Eva) #true)
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(check-expect (blue-eyed-ancestor? NP) #false)


(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or
       (blue-eyed-child?
         (child-father an-ftree))
       (blue-eyed-child?
         (child-mother an-ftree)))]))

;; ------------------------------------------------------------------
;; ------------------------------------------------------------------


(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)

(check-expect (count-persons-in-forest ff1) 2)
(check-expect (count-persons-in-forest ff2) 4)
(check-expect (count-persons-in-forest ff3) 5)

(check-expect (sum-ages-in-forest ff1 2019) 186)
(check-expect (sum-ages-in-forest ff2 2019) 293)
(check-expect (sum-ages-in-forest ff3 2019) 386)


(check-expect (average-age-in-forest ff1 2019) 93)
(check-expect (average-age-in-forest ff2 2019) 73.25)
(check-expect (average-age-in-forest ff3 2019) 77.2)


(define(blue-eyed-child-in-forest? a-forest)
  (ormap blue-eyed-child? a-forest))

;; ----------------------------------------------------------
;; ----------------------------------------------------------

(define(sum-ages-in-forest a-forest a-year)
  (foldr(lambda(x y)(+(sum-ages x a-year)y))0 a-forest))

(define(count-persons-in-forest a-forest)
  (foldr(lambda(x y)(+(count-persons x)y))0 a-forest))

(define(average-age-in-forest a-forest a-year)
  (/(sum-ages-in-forest a-forest a-year)
    (count-persons-in-forest a-forest)))


;; ----------------------------------------------------------
;; ----------------------------------------------------------

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(check-expect (count 5 'world) 0)
(check-expect (count "asd" 'world) 0)
(check-expect (count '() 'world) 0)

(define(atom? s)
  (or
   (number? s)
   (string? s)
   (symbol? s)))


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
  (local
    ((define (count-sl sl)
       (cond
         [(empty? sl) 0]
         [else
          (+ (count (first sl) sy) (count-sl (rest sl)))]))
     (define (count-atom at sy)
       (cond
         [(number? at) 0]
         [(string? at) 0]
         [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp sy)]
      [else (count-sl sexp)])))


;; ----------------------------------------------------------
;; ----------------------------------------------------------

(check-expect(depth '())1)
(check-expect(depth '(1 2 3))2)
(check-expect(depth '(1 (2 3)))3)
(check-expect(depth '(1 ((2)) 3 4))4)


(define(depth-sl sl)
  (cond
    ((empty? sl)0)
    (else(max(depth(first sl))
             (depth-sl(rest sl))))))


(define(depth sexp)
  (cond
    ((atom? sexp)1)
    (else(+ 1(depth-sl sexp)))))


;; ----------------------------------------------------------
;; ----------------------------------------------------------

(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))


(check-expect
 (substitute'(5 6 "sdf" jan michal ewa jan)'jan 'daniel)
 '(5 6 "sdf" daniel michal ewa daniel))

(check-expect
 (substitute'(5 6 "sdf" (jan michal ewa jan))'jan 'daniel)
 '(5 6 "sdf" (daniel michal ewa daniel)))

(check-expect
 (substitute'(5 6 "sdf" (jan (michal ewa jan)))'jan 'daniel)
 '(5 6 "sdf" (daniel (michal ewa daniel))))

(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))

;; ----------------------------------------------------------
;; ----------------------------------------------------------
 

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count2 sexp sy)
  (cond
    [(or(number? sexp)(string? sexp)) 0]
    [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
    [else
     (foldr(lambda(x y)(+(count2 x sy)y))0 sexp)]))
     

(check-expect (count2 'world 'hello) 0)
(check-expect (count2 '(world hello) 'hello) 1)
(check-expect (count2 '(((world) hello) hello) 'hello) 2)
(check-expect (count2 5 'world) 0)
(check-expect (count2 "asd" 'world) 0)
(check-expect (count2 '() 'world) 0)

