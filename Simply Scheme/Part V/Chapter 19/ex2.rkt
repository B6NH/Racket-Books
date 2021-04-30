; Exercise 2

#lang racket
(require (planet dyoo/simply-scheme:2:2))

(define (combine pred fst rst emp base comb)
  ((lambda (x) (x x))
   (lambda (proc-gen)
     (lambda (lst)
       (if (emp lst)
           base
           (let ((combined ((proc-gen proc-gen) (rst lst)))
                 (fe (fst lst)))
             (if (pred fe)
                 (comb fe combined)
                 combined)))))))

(define (combineword pred lst)
  ((combine pred first bf empty? "" word) lst))

(define (combinesent pred lst)
  ((combine pred car cdr null? '() se) lst))

(define (mykeep pred lst)
  ((if (word? lst) combineword combinesent) pred lst))


(and
  (word? 'element)
  (not (word? '(darker shade of black)))
  (equal? (mykeep (lambda(x)(not (equal? x 'k)))
                  'kalekidoksckopkic)
          'aleidoscopic)
  (equal? (mykeep (lambda(x)(> (count x) 2))
                  '(a bbb ab ccccc))
          '(bbb ccccc)))
