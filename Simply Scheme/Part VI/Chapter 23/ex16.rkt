; Exercise 16

#lang racket

(define (copy-list-from lst vec index)
  (if (empty? lst)
      index
      (begin
        (vector-set! vec index (car lst))
        (copy-list-from (cdr lst) vec (+ index 1)))))

(define (copy-element-from el vec index)
  (begin
    (vector-set! vec index el)
    (+ index 1)))

(define (list-to-vec lst vec index)
  (if (empty? lst)
      vec
      (let ((fst (car lst)))
        (list-to-vec
          (cdr lst)
          vec
          ((if (list? fst) copy-list-from copy-element-from)
           fst vec index)))))

(define (v-helper source target index offset)
  (if (= index (vector-length target))
      target
      (begin
        (vector-set! target index (vector-ref source (+ index offset)))
        (v-helper source target (+ index 1) offset))))

(define (real-length-helper args acc)
  (if (empty? args)
      acc
      (let ((fst (car args)))
        (real-length-helper
          (cdr args) (+ acc (if (list? fst) (length fst) 1))))))

(define (real-length args)
  (real-length-helper args 0))

(define (sentence . args)
  (list-to-vec args (make-vector (real-length args)) 0))

(define (vempty? sent)
  (zero? (vector-length sent)))

(define (vfirst sent)
  (vector-ref sent 0))

(define (v-but sent offset)
  (v-helper sent (make-vector (- (vector-length sent) 1)) 0 offset))

(define (vbutfirst sent)
  (v-but sent 1))

(define (vbutlast sent)
  (v-but sent 0))

(define (vlast sent)
  (vector-ref sent (- (vector-length sent) 1)))

(define (praise stuff)
  (sentence stuff '(is good)))

(define (praise2 stuff)
  (sentence stuff 'rules!))

; Slower
(define (item n sent)
  (if (= n 1)
      (vfirst sent)
      (item (- n 1) (vbutfirst sent))))

; Faster
(define (item-v2 n sent)
  (vector-ref sent (- n 1)))

(define (vecons el vec)
  (let ((new (make-vector (+ (vector-length vec) 1))))
    (begin
      (vector-set! new 0 el)
      (v-helper vec new 1 -1))))

(define (every fn sent)
  (if (vempty? sent)
      sent
      (vecons (fn (vfirst sent))
              (every fn (vbutfirst sent)))))

(define (every2-helper fn old new index)
  (if (= index -1)
      new
      (begin
        (vector-set! new index (fn (vector-ref old index)))
        (every2-helper fn old new (- index 1)))))

(define (every2 fn sent)
  (let ((len (vector-length sent)))
    (every2-helper fn sent (make-vector len) (- len 1))))


(define abc (sentence 'a 'b 'c))
(define mbair #(music book air))
(define ftiflower #(fish tiger flower))
(define numbers #(5 6 7))
(define new-numbers #(10 12 14))

(define (double x)
  (* x 2))

(and
  (equal? (vempty? (sentence)) #t)
  (equal? (vempty? abc) #f)
  (equal? (vfirst abc) 'a)
  (equal? (vlast abc) 'c)
  (equal? (vbutfirst abc) #(b c))
  (equal? (vbutlast abc) #(a b))
  (equal? (real-length '(water (fire air))) 3)
  (equal? (praise 'water) #(water is good))
  (equal? (praise '(water fire)) #(water fire is good))
  (equal? (praise2 'air) #(air rules!))
  (equal? (praise2 '(hello world)) #(hello world rules!))
  (equal? (item 1 mbair) 'music)
  (equal? (item-v2 1 mbair) 'music)
  (equal? (item 3 ftiflower) 'flower)
  (equal? (item-v2 3 ftiflower) 'flower)
  (equal? (every double numbers) new-numbers)
  (equal? (every2 double numbers) new-numbers))
