; Poker

#lang racket
(require (planet dyoo/simply-scheme:2:2))


(define suits '(h s c d))
(define ranks '(2 3 4 5 6 7 8 9 10 j q k a))

(define suit-names '((h hearts) (s spades) (c clubs) (d diamonds)))
(define rank-names '((2 twos)(3 threes)(4 fours)
                     (5 fifths)(6 sixes)(7 sevens)
                     (8 eights)(9 nines)(10 tens)
                     (j jacks)(q queens)(k kings)(a aces)))
(define singular-ranks '((2 two)(3 three)(4 four)
                         (5 five)(6 six)(7 seven)
                         (8 eight)(9 nine)(10 ten)
                         (j jack)(q queen)(k king)(a ace)))


(define (get-suit-name-helper symbol snames)
  (if (equal? symbol (caar snames))
      (cadar snames)
      (get-suit-name-helper symbol (cdr snames))))

(define (get-suit-name symbol)
  (get-suit-name-helper symbol suit-names))

(define (get-rank-name-helper symbol rnames)
  (if (equal? symbol (caar rnames))
      (cadar rnames)
      (get-rank-name-helper symbol (cdr rnames))))

(define (get-rank-name symbol)
  (get-rank-name-helper symbol rank-names))

(define (singular-rank-helper symbol names)
  (if (equal? symbol (caar names))
      (cadar names)
      (singular-rank-helper symbol (cdr names))))

(define (get-singular-rank symbol)
  (singular-rank-helper symbol singular-ranks))


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

(define (full-house r-data)
  (let ((three (three-kind r-data))
        (two (two-kind r-data)))
    (if (and two three)
        (se two three)
        #f)))

(define (two-pair-helper rank-data c acc)
  (cond
    ((zero? c) acc)
    ((empty? rank-data) #f)
    ((= (last (car rank-data)) 2)
     (two-pair-helper (cdr rank-data) (- c 1) (cons (first (car rank-data)) acc)))
    (else (two-pair-helper (cdr rank-data) c acc))))

(define (two-pair rank-data)
  (two-pair-helper rank-data 2 '()))

(define (two-kind r-data)
  (cond
    ((empty? r-data) #f)
    ((= (last (car r-data)) 2) (caar r-data))
    (else (two-kind (cdr r-data)))))

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
  (let ((height (five-in-row rank-data))
        (suname (get-suit-name same-suit)))
    (if height
        (if (= (last (car rank-data)) 1)
            (se 'royal 'flush '- suname)
            (se (get-singular-rank height) '- 'high 'straight 'flush '- suname))
        (if (wrap-straight rank-data)
            (se 'five '- 'high 'straight 'flush '- suname)
            (let ((val (full-house rank-data)))
              (if val
                (se 'full 'house '- (get-rank-name (cadr val))
                                    'over
                                    (get-rank-name (car val)))
                (se 'flush '- suname)))))))

(define (check-different-suit rank-data)
  (let ((val (four-kind rank-data)))
    (if val
      (se 'four 'kind '- (get-rank-name val))
      (let ((val (full-house rank-data)))
        (if val
          (se 'full 'house '- (get-rank-name (cadr val))
                              'over
                              (get-rank-name (car val)))
          (let ((height (five-in-row rank-data)))
            (if height
                (se (get-singular-rank height) '- 'high 'straight)
                (if (wrap-straight rank-data)
                    (se 'five '- 'high 'straight)
                    (let ((val (three-kind rank-data)))
                      (if val
                       (se 'three 'kind '- (get-rank-name val))
                       (let ((pairs (two-pair rank-data)))
                         (if pairs
                             (se 'two 'pair '- (get-rank-name (car pairs))
                                               'and
                                               (get-rank-name (cadr pairs)))
                             (let ((rank (two-kind rank-data)))
                               (if rank
                                   (se 'pair '- (get-rank-name rank))
                                   '(nothing)))))))))))))))

; Compute rank and suit data
; Use it later
(define (poker-value cards)
  (let ((rank-data (compute-ranks cards))
        (suit-data (compute-suits cards)))
    (let ((same-suit (all-same-suit suit-data)))
      (if same-suit
          (check-same-suit same-suit rank-data)
          (check-different-suit rank-data)))))


(and

  ; 1 - Royal flush: ten, jack, queen, king, and ace, all of the same suit
  (equal? (poker-value '(dq d10 dj da dk)) '(royal flush - diamonds))
  (equal? (poker-value '(sq sk sa sj s10)) '(royal flush - spades))

  ; 2 - Straight flush: five cards of sequential rank, all of the same suit
  (equal? (poker-value '(d5 d7 d6 d8 d4)) '(eight - high straight flush - diamonds))
  (equal? (poker-value '(h5 h2 ha h3 h4)) '(five - high straight flush - hearts))

  ; 3 - Four of a kind: four cards of the same rank
  (equal? (poker-value '(d5 h4 c5 s5 s5)) '(four kind - fifths))
  (equal? (poker-value '(cq sq dq h5 sq)) '(four kind - queens))

  ; 4 - Full house: three cards of the same rank, and two of a second rank
  (equal? (poker-value '(h4 s4 c6 s6 c4)) '(full house - fours over sixes))
  ;(equal? (poker-value '(d3 d3 dq dq d3)) '(full house - threes over queens))

  ; 5 - Flush: five cards of the same suit, not sequential rank
  (equal? (poker-value '(h3 h6 h8 hq hk)) '(flush - hearts))
  (equal? (poker-value '(c7 c4 cq c9 ck)) '(flush - clubs))

  ; 6 - Straight: five cards of sequential rank, not all of the same suit
  (equal? (poker-value '(d2 h4 d3 c6 d5)) '(six - high straight))
  (equal? (poker-value '(d7 h8 d10 c9 dj)) '(jack - high straight))

  ; 7 - Three of a kind: three cards of the same rank, no other matches
  (equal? (poker-value '(h3 d3 d8 c2 d3)) '(three kind - threes))
  (equal? (poker-value '(ca s3 sa h2 ca)) '(three kind - aces))

  ; 8 - Two pair: two pairs of cards, of two different ranks
  (equal? (poker-value '(h3 d3 d2 c2 d9)) '(two pair - twos and threes))
  (equal? (poker-value '(hq da d2 ca dq)) '(two pair - queens and aces))

  ; 9 - Pair: two cards of the same rank, no other matches
  (equal? (poker-value '(h3 d3 d8 c2 d9)) '(pair - threes))
  (equal? (poker-value '(h3 dq s9 c2 d9)) '(pair - nines))

  ; 10 - Nothing: none of the above
  (equal? (poker-value '(h2 d4 d8 ck dq)) '(nothing))
  (equal? (poker-value '(h3 d9 d7 c1 dj)) '(nothing)))


