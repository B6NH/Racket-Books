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


(and
  (equal? (vempty? (sentence)) #t)
  (equal? (vempty? (sentence 'a 'b 'c)) #f)
  (equal? (vfirst (sentence 'a 'b 'c)) 'a)
  (equal? (vlast (sentence 'a 'b 'c)) 'c)
  (equal? (vbutfirst (sentence 'a 'b 'c)) #(b c))
  (equal? (vbutlast (sentence 'a 'b 'c)) #(a b))
  (equal? (real-length '(water (fire air))) 3)
  (equal? (praise 'water) #(water is good))
  (equal? (praise '(water fire)) #(water fire is good))
  (equal? (praise2 'air) #(air rules!))
  (equal? (praise2 '(hello world)) #(hello world rules!))
  (equal? (item 1 #(music book air)) 'music)
  (equal? (item-v2 1 #(music book air)) 'music)
  (equal? (item 3 #(fish tiger flower)) 'flower)
  (equal? (item-v2 3 #(fish tiger flower)) 'flower)
  (equal? (every (lambda(x)(* x 2)) #(5 6 7)) #(10 12 14)))
