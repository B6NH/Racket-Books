; Exercise 1

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (ttt position me)
  (ttt-choose (find-triples position) me))

(define (ttt-choose triples me)
  (cond ((i-can-win? triples me))
        ((opponent-can-win? triples me))
        ((i-can-fork? triples me))
        ((i-can-advance? triples me))
        (else (best-free-square triples))))

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me)))

(define (i-can-advance? triples me)
  (best-move (keep (lambda (triple) (my-single? triple me)) triples)
             triples
             me))

(define (pivots triples me)
  (repeated-numbers
    (keep (lambda (triple) (my-single? triple me))
          triples)))

(define (best-move my-triples all-triples me)
  (if (empty? my-triples)
      #f
      (best-square (first my-triples) all-triples me) ))

(define (my-single? triple me)
  (and (= (appearances me triple) 1)
       (= (appearances (opponent me) triple) 0)))

(define (repeated-numbers sent)
  (every first
         (keep (lambda (wd) (>= (count wd) 2))
               (sort-digits (accumulate word sent)) )))

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me))
                      (keep number? my-triple)))

(define (best-square-helper opponent-pivots pair)
  (if (member? (first pair) opponent-pivots)
      (first pair)
      (last pair)))

(define (sort-digits number-word)
  (every (lambda (digit) (extract-digit digit number-word))
         '(1 2 3 4 5 6 7 8 9) ))

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd))

(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent)))

(define (find-triples position)
  (every
    (lambda (combination)
      (substitute-triple combination position))
    '(123 456 789 147 258 369 159 357)))

(define (substitute-triple combination position)
  (accumulate word
    (every (lambda (square)
             (substitute-letter square position))
           combination)))

(define (substitute-letter square position)
  (let ((element(item square position)))
    (if (equal? '_ element)
        square
        element)))

(define (opponent letter)
  (if(equal? letter 'x) 'o 'x))

(define (i-can-win? triples me)
  (choose-win
    (keep (lambda (triple) (my-pair? triple me))
          triples)))

(define (opponent-can-win? triples me)
  (i-can-win? triples (opponent me)))

(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))

(define (choose-win winning-triples)
  (if (empty? winning-triples)
      #f
      (keep number? (first winning-triples))))

(define (best-free-square triples)
  (first-choice (accumulate word triples)
                '(5 1 3 7 9 2 4 6 8)))

(define (first-choice possibilities preferences)
  (first(keep
          (lambda(square)
            (member? square possibilities))
          preferences)))

(define(already-won? position letter)
  (member? (word letter letter letter)
           (find-triples position)))


; Solution to exercise 3 by github.com/pongsh


(define (tie-game? position)
    (and (<= (appearances '_ position) 2)
         (neither-has-won position)
         (neither-can-win position)))

(define (neither-has-won position)
  (and (not (already-won? position 'x))
       (not (already-won? position 'o))))

(define (neither-can-win position)
  (let ((triples (find-triples position)))
    (not (i-can-win? triples (current-player position)))))

(define (current-player position)
  (if (> (appearances 'o position) (appearances 'x position))
      'x
      'o))


(and
  (equal? (substitute-triple 456 '_xo_x_o__) '4x6)
  (equal? (substitute-triple 147 '_xo_x_o__) '14o)
  (equal? (substitute-triple 357 '_xo_x_o__) 'oxo)
  (equal? (substitute-letter 5 '_xo_x_o__) 'x)
  (equal? (substitute-letter 8 '_xo_x_o__) 8)
  (equal? (find-triples '_xo_x_o__)
          '("1xo" "4x6" o89 "14o" xx8 o69 "1x9" oxo))
  (equal? (find-triples 'x_____oxo)
          '(x23 456 oxo x4o "25x" "36o" x5o "35o"))
  (equal? (first-choice 123456789147258369159357
                        '(5 1 3 7 9 2 4 6 8))
          5)
  (equal? (first-choice "1xo4x6o8914oxx8o691x9oxo"
                        '(5 1 3 7 9 2 4 6 8))
          1)
  (equal? (best-free-square (find-triples '_________))
          5)
  (equal? (best-free-square (find-triples '____x____))
          1)
  (equal? (ttt '____x____ 'o) 1)
  (equal? (ttt 'o__xx____ 'o) 6)
  (equal? (ttt 'o_xxxo___ 'o) 7)
  (equal? (ttt 'o_xxxoox_ 'o) 2)
  (equal? (already-won? 'o_xxxoox_ 'o) #f)
  (equal? (already-won? 'xxxoxoox_ 'x) #t)
  (equal? (tie-game? 'oxooxxxo_) #t)
  (equal? (tie-game? 'o_xxxo___) #f))
