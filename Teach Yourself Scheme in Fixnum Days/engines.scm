
; Engines

(define *engine-escape* #f)
(define *engine-entrance* #f)

; Error and "Say goodnight, cat!" message after 10 ticks
;; (clock 'set-handler
;;   (lambda ()
;;     (error "Say goodnight, cat!")))
;; (clock 'set 10)

; Set clock handler
;; (clock 'set-handler
;;   (lambda ()
;;     (call/cc *engine-escape*)))

; -------------------------------------------------------------

; Flat engine
(define make-engine
  (lambda (th)

    ; Engine is called with number of time units,
    ; succes procedure and failure procedure
    (lambda (ticks success failure)
      (let* ((ticks-left 0)
             (engine-succeeded? #f)
             (result
              (call/cc
               (lambda (k)
                 (set! *engine-escape* k)
                 (let ((result
                        (call/cc
                         (lambda (k)
                           (set! *engine-entrance* k)

                           ; Reset clock's remaining ticks to 'ticks'
                           (clock 'set ticks)
                           (let ((v (th)))
                             (*engine-entrance* v))))))

                   ; Set clock ticks to '*infinity*' and save previous value in 'ticks-left'
                   ; Clock with '*infinity*' ticks cannot run out of time
                   (set! ticks-left (clock 'set *infinity*))
                   (set! engine-succeeded? #t)
                   result)))))
        (if engine-succeeded?

            ; Apply success procedure to computation
            ; result and number of remaining ticks
            (success result ticks-left)

            ; Otherwise apply failure procedure to new engine
            ; representing unfinished portion of computation
            (failure (make-engine (lambda () (result 'resume)))))))))

; -------------------------------------------------------------

; Nestable engine
(define make-engine2
  (lambda (th)
    (lambda (ticks s f)
      (let* ((parent-ticks
              (clock 'set *infinity*))

             ; A child can’t have more ticks than its parent’s
             ; remaining ticks
             (child-available-ticks
              (clock-min parent-ticks ticks))

             ; A child’s ticks must be counted against the
             ; parent too
             (parent-ticks-left
              (clock-minus parent-ticks child-available-ticks))

             ; If child was promised more ticks than parent
             ; could afford, remember how much it was
             ; short-changed by
             (child-ticks-left
              (clock-minus ticks child-available-ticks))

             ; Used below to store ticks left in clock
             ; if child completes in time
             (ticks-left 0)

             (engine-succeeded? #f)

             (result
              (fluid-let ((*engine-escape* #f)
                          (*engine-entrance* #f))
                (call/cc
                 (lambda (k)
                   (set! *engine-escape* k)
                   (let ((result
                          (call/cc
                           (lambda (k)
                             (set! *engine-entrance* k)
                             (clock 'set
                               child-available-ticks)

                             (let ((v (th)))

                               (*engine-entrance* v))))))
                     (set! ticks-left
                       (let ((n (clock 'set *infinity*)))
                         (if (eqv? n *infinity*) 0 n)))
                     (set! engine-succeeded? #t)
                     result))))))

        ; Parent can reclaim ticks that child didn’t need
        (set! parent-ticks-left
          (clock-plus parent-ticks-left ticks-left))

        ; This is the true ticks that child has left —
        ; we include the ticks it was short-changed by
        (set! ticks-left
          (clock-plus child-ticks-left ticks-left))

        ; Restart parent with its remaining ticks
        (clock 'set parent-ticks-left)
        ; The rest is now parent computation

        (cond
         ; Child finished in time — celebrate its success
         (engine-succeeded? (s result ticks-left))

         ; Child failed because it ran out of promised time —
         ; call failure procedure
         ((= ticks-left 0)
          (f (make-engine2 (lambda () (result 'resume)))))

         ; Child failed because parent didn’t have enough time,
         ; i.e., parent failed too.  If so, when parent is
         ; resumed, its first order of duty is to resume the
         ; child with its fair amount of ticks
         (else
          ((make-engine2 (lambda () (result 'resume)))
           ticks-left s f)))))))

; -------------------------------------------------------------

; Flat engine with procedure that prints non-negative integers
(define printn-engine
  (make-engine
    (lambda ()
      (let loop ((i 0))
        (display i)
        (display " ")
        (loop (+ i 1))))))

; -------------------------------------------------------------

; Call printn-engine
; Failure procedure lambda sets '*more*' to new engine
;; (define *more* #f)
;; (printn-engine 50 list (lambda (ne) (set! *more* ne)))

; New engine can be used to resume computation
;; (*more* 50 list (lambda (ne) (set! *more* ne)))
