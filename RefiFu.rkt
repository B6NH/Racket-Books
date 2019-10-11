;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname RefiFu) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

(define W(create-dir "/var/log/folderracket"))

(define OUTPUT-DIR
  (make-dir
   '/var/log/folderracket
   (list
    (make-dir '/var/log/folderracket/emptyone '() '())
    (make-dir
     '/var/log/folderracket/insideracket
     '()
     (list
      (make-file "def" 15 (make-date 2019 3 21 15 33 53) "")
      (make-file "insidefile" 56 (make-date 2019 3 21 15 34 3) ""))))
   (list
    (make-file "abc" 427 (make-date 2019 3 21 15 33 0) "")
    (make-file "def" 4 (make-date 2019 3 21 15 33 24) ""))))
  
(define(sum lst)
  (foldr(lambda(x y)(+ x y))0 lst))

(define(how-many-in-files files file)
  (sum(map(lambda(x)(if(string=? file(file-name x))1 0))files)))


(define(how-many-in-dirs dirs file)
  (sum(map(lambda(x)(how-many x file))dirs)))


(define(how-many dir file)
  (+(how-many-in-files(dir-files dir)file)
    (how-many-in-dirs(dir-dirs dir)file)))


(check-expect(how-many W "def")2)
(check-expect(how-many W "abc")1)
(check-expect(how-many W "sss")0)

(define(find-in-files? files file)
  (ormap(lambda(x)(string=?(file-name x)file))files))

(define(find-in-dirs? dirs file)
  (ormap(lambda(x)(find? x file))dirs))

(define(find? dir file)
  (or(find-in-files?(dir-files dir)file)
     (find-in-dirs?(dir-dirs dir)file)))

(check-expect(find? W "abc")#true)
(check-expect(find? W "def")#true)
(check-expect(find? W "insidefile")#true)
(check-expect(find? W "sss")#false)


(define(show-all-filenames files)
  (map(lambda(x)(file-name x))files))

(define(show-all-dirnames dirs)
  (map(lambda(x)(dir-name x))dirs))


(define(ls dir)
  (append(show-all-filenames(dir-files dir))
         (show-all-dirnames(dir-dirs dir))))

(check-expect
 (ls W)
 (ls OUTPUT-DIR))

(check-expect
 (ls W)
 (list "abc" "def" '/var/log/folderracket/emptyone '/var/log/folderracket/insideracket))


(define(files-size dir)
  (sum(map(lambda(x)(file-size x))
          (dir-files dir))))

;; size of all files, directory size is 1
(define(du dir)
  (+(files-size dir)
    (sum(map files-size(dir-dirs dir)))
    (length(dir-dirs dir))))

(check-expect
 (du W)
 (+ 427 4 1 1 15 56))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------


(define(find dir file)
  (local
    ((define(create-path-to dir)
       (cons(symbol->string(dir-name dir))
            (if(find-in-files?(dir-files dir)file)
               (list file)
               (find-in-dirs(dir-dirs dir)))))
     (define(find-in-dirs dirs)
       (if(find?(car dirs)file)
          (find(car dirs)file)
          (find-in-dirs(rest dirs)))))
    (if(find? dir file)
       (create-path-to dir)
       #false)))

(check-expect
 (find W "insidefile")
 (list "/var/log/folderracket" "/var/log/folderracket/insideracket" "insidefile"))

(check-expect
 (find W "def")
 (list "/var/log/folderracket" "def"))

(check-expect
 (find W "zzz")
 #false)

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------


(define(create-paths-from-dirs dirs file)
  (foldr(lambda(x y)(append(create-paths-from-dir x file)y))'()dirs))

(define(add-prefix name paths)
  (map(lambda(x)(cons(symbol->string name)x))paths))

(define(find-in-dir-names? dirs name)
  (ormap(lambda(x)(string=?(symbol->string(dir-name x))name))dirs))
 
;; this is find-all function
(define(create-paths-from-dir dir file-or-dir)
  (local
    ((define next-paths
       (create-paths-from-dirs(dir-dirs dir)file-or-dir)))
    (add-prefix
     (dir-name dir)
     (cond
       ((find-in-files?(dir-files dir)file-or-dir)
        (cons(list file-or-dir)next-paths))
       ((find-in-dir-names?(dir-dirs dir)file-or-dir)
        (cons(list file-or-dir)next-paths))
        (else next-paths)))))


;; from IRC
#|
(define (create-paths-from-dir dir file)
    (if (find-in-files? (dir-files dir) file)
        (cons (list (symbol->string (dir-name dir)) file)
              (create-paths-from-dirs (dir-dirs dir) file))
        (create-paths-from-dirs (dir-dirs dir) file)))

(define(create-paths-from-dirs dirs file)
  (map(lambda (x)
        (create-paths-from-dir x file))
      dirs))

|#

(check-expect
 (create-paths-from-dir W "sss")
 '())

(check-expect
 (create-paths-from-dir W "abc")
 (list (list "/var/log/folderracket" "abc")))

(check-expect
 (create-paths-from-dir W "def")
 (list
  (list "/var/log/folderracket" "def")
  (list "/var/log/folderracket" "/var/log/folderracket/insideracket" "def")))


(check-expect
 (create-paths-from-dir W "/var/log/folderracket/insideracket")
 (list
  (list "/var/log/folderracket" "/var/log/folderracket/insideracket")))


;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------


(define(create-file-paths files)
  (map(lambda(x)(list(file-name x)))files))

(define(create-all-dir-paths dirs)
  (foldr(lambda(x y)(append(ls-R x)y))'()dirs))

(define(ls-R dir)
  (map(lambda(x)(cons(symbol->string(dir-name dir))x))
      (append(create-file-paths(dir-files dir))
             (create-all-dir-paths(dir-dirs dir)))))


(check-expect
 (ls-R W)
 (list
  (list "/var/log/folderracket" "abc")
  (list "/var/log/folderracket" "def")
  (list "/var/log/folderracket" "/var/log/folderracket/insideracket" "def")
  (list "/var/log/folderracket" "/var/log/folderracket/insideracket" "insidefile")))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

(check-expect
 (find-all W "sss")
 '())

(check-expect
 (find-all W "abc")
 (list (list "/var/log/folderracket" "abc")))

(check-expect
 (find-all W "def")
 (list
  (list "/var/log/folderracket" "def")
  (list "/var/log/folderracket" "/var/log/folderracket/insideracket" "def")))

(check-expect
 (find-all W "/var/log/folderracket/insideracket")
 (list
  (list "/var/log/folderracket" "/var/log/folderracket/insideracket")))

(check-expect
 (find-all W "/var/log/folderracket")
 '())

(check-expect
 (find-all-for-dirs W "/var/log/folderracket/insideracket")
 (list
  (list "/var/log/folderracket" "/var/log/folderracket/insideracket")))

(check-expect
 (find-all-for-files W "/var/log/folderracket/insideracket")
 '())

(define(reverse-all-items lst)
  (map(lambda(x)(reverse x))lst))


(define(find-all-for-files dir file)
  (reverse-all-items
   (filter
    (lambda(x)(string=?(first x)file))
    (reverse-all-items(ls-R dir)))))


(define(create-dir-paths-from-dirs dirs file)
  (foldr(lambda(x y)(append(find-all-for-dirs x file)y))'()dirs))

(define(find-all-for-dirs dir dirname)
  (local
    ((define next-paths
       (create-dir-paths-from-dirs(dir-dirs dir)dirname)))
    (add-prefix
     (dir-name dir)
     (if(find-in-dir-names?(dir-dirs dir)dirname)
        (cons(list dirname)next-paths)
        next-paths))))


(define(find-all dir name)
  (append(find-all-for-files dir name)
         (find-all-for-dirs dir name)))


