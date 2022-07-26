
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

(define reverse!
  (lambda (s)

    ; Initially, r is empty list
    (let loop ((s s) (r '()))

      ; Return inverted list
      (if (null? s)
          r

          ; Save value of (cdr s), which in next iteration will become new s
          (let ((d (cdr s)))

            ; Variable s becomes new r with old r as cdr
            ; '(1 2 3) '() -> '(1),
            ; '(2 3) '(1) -> '(2 1),
            ; '(3) '(2 1) -> '(3 2 1)
            (set-cdr! s r)

            ; Loop with d as new s and s as new r
            (loop d s))))))

