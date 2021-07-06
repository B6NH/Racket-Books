; Poker

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define hand-size 5)
(define suits '(h s c d))
(define ranks '(2 3 4 5 6 7 8 9 10 j q k a))

(define (all-same-suit s-data)
  (if (empty? s-data)
      #f
      (let ((c (cadar s-data)))
        (cond
          ((= c 5) (caar s-data))
          ((= c 0) (all-same-suit (cdr s-data)))
          (else #f)))))

(define (count-suit suit cards)
  (if (empty? cards)
      0
      (+ (if (equal? suit (first(car cards))) 1 0)
         (count-suit suit (cdr cards)))))

(define (compute-ranks-helper cards rnks acc)
  (if (empty? rnks)
       acc
      (compute-ranks-helper
        cards
        (cdr rnks)
        (cons (cons (car rnks)(cons (count-rank (car rnks) cards) '())) acc))))

(define (compute-suits-helper cards sts acc)
  (if (empty? sts)
       acc
      (compute-suits-helper
        cards
        (cdr sts)
        (cons (cons (car sts)(cons (count-suit (car sts) cards) '())) acc))))

(define (compute-ranks cards)
  (compute-ranks-helper cards ranks '()))

(define (compute-suits cards)
  (compute-suits-helper cards suits '()))

(define (count-rank rank cards)
  (if (empty? cards)
      0
      (let ((rank-symbol (if (equal? rank 10) 0 rank)))
        (+ (if (equal? rank-symbol (last(car cards))) 1 0)
           (count-rank rank-symbol (cdr cards))))))

(define (five-helper r-data c)
  (cond
    ((= c 5) #t)
    ((= (last (car r-data)) 1)
     (five-helper (cdr r-data) (+ c 1)))
    (else #f)))

(define (five-in-row r-data)
  (cond
    ((< (length r-data) 5) #f)
    ((five-helper r-data 0) (caar r-data))
    (else (five-in-row (cdr r-data)))))

(define (four-in-row r-data)
  (cond
    ((empty? r-data) #t)
    ((= (last (car r-data)) 1)
     (four-in-row (cdr r-data)))
    (else #f)))


(define (last-four r-data)
  (if (> (length r-data) 4)
      (last-four (cdr r-data))
      (four-in-row r-data)))

; (ace, 2, 3, 4, 5)
(define (wrap-straight r-data)
  (and (= (last (car r-data)) 1)
       (last-four r-data)))

(define (full-house rank-data)
  #f)

(define (two-pair rank-data)
  #f)

(define (pair rank-data)
  #f)

(define (three-kind r-data)
  (cond
    ((empty? r-data) #f)
    ((= (last (car r-data)) 3) (caar r-data))
    (else (three-kind (cdr r-data)))))

(define (four-kind r-data)
  (cond
    ((empty? r-data) #f)
    ((= (last (car r-data)) 4) (caar r-data))
    (else (four-kind (cdr r-data)))))

(define (check-same-suit same-suit rank-data)
  (let ((height (five-in-row rank-data)))
    (if height
        (if (= (last (car rank-data)) 1)
            (se 'royal 'flush '- same-suit)
            (if (four-kind rank-data)
                (se 'four 'kind)
                (se height '- 'high 'straight 'flush)))
        (if (wrap-straight rank-data)
            (se 5 '- 'high 'straight 'flush)
            (cond
              ((four-kind rank-data) (se 'four 'kind))
              ((full-house rank-data) (se 'full 'house))
              (else (se 'flush same-suit)))))))

(define (check-different-suit rank-data)
  (cond
    ((four-kind rank-data) (se 'four 'kind))
    ((full-house rank-data) (se 'full 'house))
    (else
      (let ((height (five-in-row rank-data)))
        (if height
            (se height '- 'high 'straight)
            (cond
              ((wrap-straight rank-data)
               (se 5 '- 'high 'straight))
              ((three-kind rank-data)(se 'three 'kind))
              ((two-pair rank-data)(se 'two 'pair))
              ((pair rank-data)(se 'pair))
              (else 'nothing)))))))

; Compute rank and suit data
; Use it later
(define (poker-value cards)
  (let ((rank-data (compute-ranks cards))
        (suit-data (compute-suits cards)))
    (let ((same-suit (all-same-suit suit-data)))
      (if same-suit
          (check-same-suit same-suit rank-data)
          (check-different-suit rank-data)))))


(equal? (poker-value '(dq d10 dj da dk)) '(royal flush - d))

(equal? (poker-value '(d5 d7 d6 d8 d4)) '(8 - high straight flush))
(equal? (poker-value '(d5 d2 da d3 d4)) '(5 - high straight flush))

(equal? (poker-value '(d5 h4 c5 s5 s5)) '(four kind))

(equal? (poker-value '(d2 h4 d3 c6 d5)) '(6 - high straight))

