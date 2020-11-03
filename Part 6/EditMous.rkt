;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname EditMous) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
;; 32.4 A Graphical Editor, with Mouse

(define FONT-SIZE 16)
(define FONT-COLOR "black")
(define WIDTH 200)
(define RIGHT-MARGIN 10)
(define HEIGHT 20)
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))
(define MT (empty-scene WIDTH HEIGHT))

(define-struct editor [pre post])

(define(editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

(define(add-at-end lst s)
  (cond
    ((empty? lst)(cons s '()))
    (else(cons(first lst)
              (add-at-end(rest lst)s)))))

(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

(define(create-editor one two)
  (make-editor (rev(explode one)) (explode two)))

(define(editor-lft ed)
  (if(empty?(editor-pre ed))
     ed
     (make-editor(rest(editor-pre ed))
                 (cons(first(editor-pre ed))(editor-post ed)))))

(define(editor-rgt ed)
  (if(empty?(editor-post ed))
     ed
     (make-editor(cons(first(editor-post ed))
                      (editor-pre ed))
                 (rest(editor-post ed)))))

(define (editor-del ed)
  (if(empty?(editor-pre ed))
     ed
     (make-editor(rest(editor-pre ed))
                 (editor-post ed))))

(define (editor-ins ed k)
  (if(>=(+(image-width(editor-text(editor-pre ed)))
          (image-width(editor-text(editor-post ed))))
       (- WIDTH RIGHT-MARGIN))
     ed
     (make-editor (cons k (editor-pre ed))
                  (editor-post ed))))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

(define(editor-render ed)
  (place-image/align
   (beside (editor-text(rev(editor-pre ed)))
           CURSOR
           (editor-text(editor-post ed)))
   1 1
   "left" "top"
   MT))

(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]
     [on-mouse mouse-click]))


(define(get-text-width x)(image-width(editor-text x)))

(define(take x lst)
  (cond
    ((zero? x)'())
    (else(cons(first lst)
              (take(sub1 x)(rest lst))))))


(define(split-structural lst x)
  (local((define width(image-width(editor-text lst)))
         (define left-w x)
         (define right-w(- width left-w))
         (define(h letters)
           (cond
             ((empty? letters)'())
             ((<=(get-text-width(rest letters))right-w)letters)
             (else(h(rest letters)))))
         (define rightpart(h lst))
         (define leftpart(take(-(length lst)(length rightpart))lst)))
    (make-editor (reverse leftpart) rightpart)))



(define(mouse-click ed x y res)
  (if(string=? res "button-down")
     (split(append(reverse(editor-pre ed))
                  (editor-post ed))x)
     ed))

;; Exercise 508. Design split-structural using the structural

(define x 30)

(define splitted(split-structural '("a" "b" "c" "d" "e" "f")x))

(define p(editor-pre splitted))
(define s(editor-post splitted))

(check-expect(<= (image-width (editor-text p))
                 x
                 (image-width (editor-text (append p(list(first s))))))#true)


;; Exercise 509. Design the function split

(define(split lst x)
  (local((define width(image-width(editor-text lst)))
         (define left-w x)
         (define right-w(- width left-w))
         (define(h letters acc)
           (cond
             ((empty? letters)(make-editor acc '()))
             ((<=(get-text-width(rest letters))right-w)(make-editor acc letters))
             (else(h(rest letters)(cons(first letters)acc))))))
    (h lst '())))


(define splitted2(split '("a" "b" "c" "d" "e" "f")x))

(define p2(editor-pre splitted2))
(define s2(editor-post splitted2))

(check-expect(<= (image-width (editor-text p2))
                 x
                 (image-width (editor-text (append p2(list(first s2))))))#true)



(check-expect(split '("a" "b" "c" "d" "e" "f")x)
             (split-structural '("a" "b" "c" "d" "e" "f")x))

;;(main "abc")


;; Exercise 510. Many operating systems come with the fmt program


;(define input(read-words "fmt_input.txt"))


(define(transform input max-size)
  (local((define(h lst acc)
           (cond
             ((empty? lst)"")
             (else(local((define f(first lst))
                         (define fst-l(string-length f))
                         (define new-acc(+ acc fst-l)))
                    (if(> new-acc max-size)
                       (string-append "\n"(h lst 0))
                       (string-append(if(= acc 0)"" " ")f(h(rest lst)new-acc))))))))
    (h input 0)))


(check-expect
 (transform(list "komnata" "w" "zamku")7)
 "komnata\nw zamku")
(check-expect
 (transform(list "komnata" "w" "zamku" "ksiecia" "wszystko" "jest")8)
 "komnata w\nzamku\nksiecia\nwszystko\njest")


(define(fmt w in-f out-f)
  (write-file out-f(transform(read-words in-f)w)))

;;(fmt 20 "fmt_input.txt" "fmt_output.txt")
