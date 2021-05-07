; Exercise 5

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (helper names sofar)
  (if (empty? names)
      sofar
      (let ((len (count (cadar names))))
        (helper (cdr names)
                (if (> len sofar) len sofar)))))

(define (longest-lname names)
  (helper (cdr names) (count (cadar names))))

(define (mhelper names width)
  (if (null? names)
      'done
      (begin
        (display (align (cadar names) width))
        (show (caar names))
        (mhelper (cdr names) width))))

(define (name-table names)
  (mhelper names (+ (longest-lname names) 2)))


(define british '((john lennon) (paul mccartney)
                  (george harrison) (ringo starr)))

(define russian '((piotr tchaikovsky) (nicolay rimsky-korsakov)
                  (sergei rachmaninov) (modest musorgsky)))

(define jazz '((bill evans) (paul motian) (scott lefaro)))

(define d 'done)


(and
  (equal? (name-table british) d)
  (equal? (name-table russian) d)
  (equal? (name-table jazz) d))


