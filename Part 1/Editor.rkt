;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 


(define(add-at-end lst s)
  (cond
    ((empty? lst)(cons s '()))
    (else(cons(first lst)
              (add-at-end(rest lst)s)))))
  
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))


(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))


(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))
;; ----------------------------------------------------------------

(define(create-editor one two)
  (make-editor (rev(explode one)) (explode two)))

(check-expect(create-editor "abc"
                            "def")
             (make-editor (list  "c" "b" "a")
                          (list "d" "e" "f")))

;; ----------------------------------------------------------------
(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define RIGHT-MARGIN 10)
;; ----------------------------------------------------------------
(define(editor-lft ed)
  (if(empty?(editor-pre ed))
     ed
     (make-editor(rest(editor-pre ed))
                 (cons(first(editor-pre ed))(editor-post ed)))))


(check-expect(editor-lft(make-editor (cons "e" (cons "d" '()))
                                     (cons "f" (cons "g" '()))))
             (make-editor (cons "d" '())
                          (cons "e"(cons "f" (cons "g" '())))))


;; cant move left when left part is empty
(check-expect(editor-lft(make-editor '()
                                     (cons "f" (cons "g" '()))))
             (make-editor '()
                          (cons "f" (cons "g" '()))))

;; ----------------------------------------------------------------
(define(editor-rgt ed)
  (if(empty?(editor-post ed))
     ed
     (make-editor(cons(first(editor-post ed))
                      (editor-pre ed))
                 (rest(editor-post ed)))))

(check-expect(editor-rgt(make-editor (cons "e" (cons "d" '()))
                                     (cons "f" (cons "g" '()))))
             (make-editor (cons "f"(cons "e" (cons "d" '())))
                          (cons "g" '())))

;; cant move right when post part is empty
(check-expect(editor-rgt(make-editor (cons "f" (cons "g" '()))
                                     '()))
             (make-editor (cons "f" (cons "g" '()))
                          '()))

;; ----------------------------------------------------------------

(define (editor-del ed)
  (if(empty?(editor-pre ed))
     ed
     (make-editor(rest(editor-pre ed))
                 (editor-post ed))))

(check-expect(editor-del(make-editor (cons "e" (cons "d" '()))
                                     (cons "f" (cons "g" '()))))
             (make-editor (cons "d" '())
                          (cons "f" (cons "g" '()))))

;; cant del when pre part is empty
(check-expect(editor-del(make-editor '()
                                     (cons "f" (cons "g" '()))))
             (editor-del(make-editor '()
                                     (cons "f" (cons "g" '())))))

;; ----------------------------------------------------------------
(define (editor-ins ed k)
  (if(>=(+(image-width(editor-text(editor-pre ed)))
          (image-width(editor-text(editor-post ed))))
       (- WIDTH RIGHT-MARGIN))
     ed
     (make-editor (cons k (editor-pre ed))
                  (editor-post ed))))


(define longlist
  (list "a" "a" "a" "a" "a" "a" 
        "a" "a" "a" "a" "a"))

(check-expect(editor-ins (make-editor longlist longlist)"k")
             (make-editor longlist longlist))
     

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
 (editor-ins
  (make-editor (cons "d" '())
               (cons "f" (cons "g" '())))
  "e")
 (make-editor (cons "e" (cons "d" '()))
              (cons "f" (cons "g" '()))))

;; ----------------------------------------------------------------
(define(myimplode s)
  (cond
    ((empty? s)"")
    (else(string-append(first s)
                       (myimplode(rest s))))))

(define(editor-text s)
  (text (myimplode s) FONT-SIZE FONT-COLOR))


(check-expect(editor-text (list "a" "b" "c"))
             (text "abc" FONT-SIZE FONT-COLOR))

;; ----------------------------------------------------------------

(define(editor-render ed)
  (place-image/align
   (beside (editor-text(rev(editor-pre ed)))
           CURSOR
           (editor-text(editor-post ed)))
   1 1
   "left" "top"
   MT))

(check-expect
 (editor-render (create-editor "pre" "post"))
 (place-image/align
  (beside (text "pre" FONT-SIZE FONT-COLOR)
          CURSOR
          (text "post" FONT-SIZE FONT-COLOR))
  1 1
  "left" "top"
  MT))
      
;; ----------------------------------------------------------------       

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))

(check-expect
 (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))

 
; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

;;(main "abc")

