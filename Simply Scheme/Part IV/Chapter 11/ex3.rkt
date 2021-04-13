; Exercise 3

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (unspell-letter letter)
  (cond ((member? letter 'abc) 2)
  ((member? letter 'def) 3)
  ((member? letter 'ghi) 4)
  ((member? letter 'jkl) 5)
  ((member? letter 'mno) 6)
  ((member? letter 'prs) 7)
  ((member? letter 'tuv) 8)
  ((member? letter 'wxy) 9)
  (else 0)))


(define (phone-unspell wd)
  (if (empty? wd)
      ""
      (word(unspell-letter(first wd))
           (phone-unspell(bf wd)))))

(equal? (phone-unspell 'popcorn) 7672676)
