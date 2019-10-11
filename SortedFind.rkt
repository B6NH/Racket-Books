;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname SortedFind) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (sorted? cmp l)
  (cond
    ((empty?(rest l))#true)
    (else(and(cmp(first l)(second l))
             (sorted? cmp(rest l))))))


;; check if all elements from k are in l
(define (contains? l k)
  (andmap (lambda (item-from-k) (member? item-from-k l)) k))

(define(sorted-variant-of k cmp)
  (lambda(l)
    (and(sorted? cmp l)
        (contains? l k)
        (contains? k l))))

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
(check-expect [(sorted-variant-of '(3 2) <) '(2 3)]
              #true)
(check-expect [(sorted-variant-of '(3 2) <) '(3)]
              #false)

;; --------------------------------------------------------
;; --------------------------------------------------------
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))


(define(found? x lst)
  (lambda(res)(and(= x(first res))
                  (<=(length res)(length lst))
                  (contains? lst res))))

(check-satisfied(find 5 '(2 3 4 5 6 7 8))
                (found? 5 '(2 3 4 5 6 7 8)))