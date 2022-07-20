
; Calculate average value returned by 'experiment' function
(define monte-carlo
  (lambda (experiment)
    (let loop ((i 0) (acc 0.0))
      (if (= i *num-trials*)
          (/ acc *num-trials*)
          (loop (+ i 1) (+ acc (experiment)))))))

(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (cond
       ((null? l) #f)
       ((eqv? (car l) o) i)
       (else (loop (+ i 1) (cdr l)))))))

