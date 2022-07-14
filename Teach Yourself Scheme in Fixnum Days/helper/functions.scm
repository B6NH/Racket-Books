
(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (cond
       ((null? l) #f)
       ((eqv? (car l) o) i)
       (else (loop (+ i 1) (cdr l)))))))

