; Numbers

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial(- n 1)))))

(define small-numbers
  '(zero one two three four five six seven eight nine))

(define teens
  '(ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))

(define decs
  '(twenty thirty forty fifty sixty seventy eighty ninety))

(define large-numbers
  '(() thousand million billion trillion quadrillion quintillion
    sextillion septillion octillion nonillion decillion))

(define (mtake w n)
  (if (= n 0)
      '()
      (se (first w)
          (mtake (bf w) (- n 1)))))

(define (mdrop w n)
  (if (= n 0)
      w
      (mdrop (bf w) (- n 1))))

(define (process-groups groups index)
  (if (empty? groups)
      '()
      (let ((grp (mtake groups 3)))
        (se (process-group grp)
            (if (equal? grp '(0 0 0))
                '()
                (get-group-name index))
            (process-groups (mdrop groups 3) (- index 1))))))


(define (get-small-number n)
  (if (zero? n)
      '()
      (get-small n)))

(define (get-small n)
  (item (+ n 1) small-numbers))

(define (get-teen n)
  (item (+ n 1) teens))

(define (get-dec n)
  (item (- n 1) decs))

(define (process-two group)
  (let ((fst (first group)) (lg (last group)))
    (cond
      ((zero? fst) (get-small-number lg))
      ((= fst 1) (get-teen lg))
      (else (se (get-dec fst)
                (get-small-number lg))))))

(define (process-three group)
  (let ((fst (first group)))
    (se
      (if (not (zero? fst))
          (se (get-small fst) 'hundred)
          '())
      (process-group (bf group)))))

(define (process-group group)
    (let ((len (count group)))
      (cond
        ((zero? len) '())
        ((= len 1) (item (+ (first group) 1) small-numbers))
        ((= len 2) (process-two group))
        ((= len 3) (process-three group)))))

(define (get-group-name index)
  (item index large-numbers))

(define (get-initial-index remsize)
  (+ (/ remsize 3) 1))

(define (number-name n)
  (let ((size (count n)))
    (let ((todrop (remainder size 3)))
      (let ((index (get-initial-index (- size todrop))))
        (se (process-group (mtake n todrop))
            (if (not(zero? todrop))
                (get-group-name index)
                '())
            (process-groups
              (mdrop n todrop)
              (- index 1)))))))

(and
  (equal? (number-name 0) '(zero))
  (equal? (number-name 5) '(five))
  (equal? (number-name 9) '(nine))
  (equal? (number-name 10) '(ten))
  (equal? (number-name 15) '(fifteen))
  (equal? (number-name 25) '(twenty five))
  (equal? (number-name 69) '(sixty nine))
  (equal? (number-name 95) '(ninety five))
  (equal? (number-name 100) '(one hundred))
  (equal? (number-name 105) '(one hundred five))
  (equal? (number-name 115) '(one hundred fifteen))
  (equal? (number-name 125) '(one hundred twenty five))
  (equal? (number-name 246) '(two hundred forty six))
  (equal? (number-name 312) '(three hundred twelve))
  (equal? (number-name 360) '(three hundred sixty))
  (equal? (number-name 999) '(nine hundred ninety nine))
  (equal? (number-name 1000) '(one thousand))
  (equal? (number-name 1001) '(one thousand one))
  (equal? (number-name 1017) '(one thousand seventeen))
  (equal? (number-name 1817) '(one thousand eight hundred seventeen))
  (equal? (number-name 5678) '(five thousand six hundred seventy eight))
  (equal? (number-name 23229) '(twenty three thousand two hundred twenty nine))
  (equal? (number-name 664593) '(six hundred sixty four thousand five hundred ninety three))
  (equal? (number-name 1000000) '(one million))
  (equal? (number-name 1000017) '(one million seventeen))
  (equal? (number-name 1000005) '(one million five))
  (equal? (number-name 1000526) '(one million five hundred twenty six))
  (equal? (number-name 3234567)
    '(three million two hundred thirty four thousand five hundred sixty seven))
  (equal? (number-name (factorial 20))
    '(two quintillion four hundred thirty two quadrillion nine hundred two
      trillion eight billion one hundred seventy six million six hundred
      forty thousand))
  (equal? (number-name 5513345)
    '(five million five hundred thirteen thousand three hundred forty five)))
