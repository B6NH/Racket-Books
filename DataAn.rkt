;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname DataAn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 20.1 Data Analysis


(define TS
  '(("part1" "part2" "part3")
    "read"
    (("hang" "draw")
     ("read"))))


(check-expect(how-many TS "read")2)
(check-expect(how-many TS "hang")1)
(check-expect(how-many TS "purple")0)
(check-expect(how-many TS "")0)

#|
(define(how-many dir file)
  (cond
    ((empty? dir)0)
    ((string?(first dir))
     (+(if(string=? file(first dir))1 0)
       (how-many(rest dir)file)))
    (else(+(how-many(first dir)file)
           (how-many(rest dir)file)))))

|#

(define(how-many dir file)
  (foldr
   (lambda(x y)
     (if(string? x)
        (+(if(string=? file x)1 0) y)
        (+(how-many x file)y)))0 dir))

;; -----------------------------------------------------------------
;; -----------------------------------------------------------------

(check-expect(how-many2 TS2 "read")2)
(check-expect(how-many2 TS2 "hang")1)
(check-expect(how-many2 TS2 "purple")0)
(check-expect(how-many2 TS2 "")0)

(define-struct dir [name content])

(define TS2
  (make-dir "TS"
            (list(make-dir "Text"(list "part1" "part2" "part3"))
                 "read"
                 (make-dir "Libs"
                           (list(make-dir "Code"(list "hang" "draw"))
                                (make-dir "Docs"(list "read")))))))
            

#|
(define(how-many2 dir file)
  (cond
    ((empty?(dir-content dir))0)
    ((string?(first(dir-content dir)))
     (+(if(string=? file(first(dir-content dir)))1 0)
       (how-many2(make-dir(dir-name dir)
                          (rest(dir-content dir)))file)))
    (else(+(how-many2(first(dir-content dir))file)
           (how-many2(make-dir(dir-name dir)
                              (rest(dir-content dir)))file)))))

|#

(define(sum lst)
  (foldr(lambda(x y)(+ x y))0 lst))

(define(how-many2 dir file)
  (sum
   (map(lambda(x)
         (if(string? x)
            (if(string=? file x) 1 0)
            (how-many2 x file)))
       (dir-content dir))))

;; -----------------------------------------------------------------
;; -----------------------------------------------------------------

(define-struct file [name size content])
(define-struct dir3 [name dirs files])

(define TS3
  (make-dir3 "TS"
             (list(make-dir3 "Text" '() (list(make-file "part1" 5 "")
                                             (make-file "part2" 7 "")
                                             (make-file "part3" 3 "")))
                  (make-dir3 "Libs"
                             (list(make-dir3 "Code" '()
                                             (list(make-file "hang" 2 "")
                                                  (make-file "draw" 9 "")))
                                  (make-dir3 "Docs" '()(list(make-file "read" 13 ""))))
                                  '()))
             (list(make-file "read" 10 ""))))


(define example-files
  (list(make-file "read" 6 "")
       (make-file "fly" 2 "")
       (make-file "read" 3 "")))
(check-expect
 (how-many-in-files
  (dir3-files TS3)
  "read")1)
(check-expect
 (how-many-in-files
  (dir3-files TS3)
  "hang")0)
(check-expect
 (how-many-in-files
  example-files
  "read")2)
(check-expect
 (how-many-in-files
  example-files
  "fly")1)
(check-expect
 (how-many-in-files
  example-files
  "run")0)


(define(how-many-in-files files file)
  (sum(map(lambda(x)(if(string=? file(file-name x))1 0))files)))


(define(how-many-in-dirs dirs file)
  (sum(map(lambda(x)(how-many3 x file))dirs)))


(define(how-many3 dir file)
  (+(how-many-in-files(dir3-files dir)file)
    (how-many-in-dirs(dir3-dirs dir)file)))


(check-expect(how-many3 TS3 "read")2)
(check-expect(how-many3 TS3 "hang")1)
(check-expect(how-many3 TS3 "air")0)


