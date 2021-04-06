;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname InterEv) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 21.1 Interpreting Expressions
(define-struct add [left right])
(define-struct mul [left right])
(define-struct my-or [left right])
(define-struct my-and [left right])
(define-struct my-not [boolean])

(define(eval-expression expression)
  (cond
    ((number? expression)expression)
    ((mul? expression)
     (*(eval-expression(mul-left expression))
       (eval-expression(mul-right expression))))
    ((add? expression)
     (+(eval-expression(add-left expression))
       (eval-expression(add-right expression))))))

(check-expect(eval-expression 52) 52)
(check-expect(eval-expression(make-add 5 6))11)
(check-expect(eval-expression(make-mul 5 6))30)
(check-expect
 (eval-expression
  (make-mul(make-add 2 3)(make-mul 5 3)))75)
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Bool Expressions

(check-expect(eval-bool-expression #true)#true)
(check-expect(eval-bool-expression(make-my-or #true #false))#true)
(check-expect(eval-bool-expression(make-my-or #false #true))#true)
(check-expect(eval-bool-expression(make-my-or #false #false))#false)
(check-expect
 (eval-bool-expression
  (make-my-or #true
              #false))#true)
(check-expect
 (eval-bool-expression
  (make-my-or #false
              #false))#false)
(check-expect
 (eval-bool-expression
  (make-my-and #true
               #false))#false)
(check-expect
 (eval-bool-expression
  (make-my-or(make-my-or #false #false)
             (make-my-or #false #true)))#true)
(check-expect
 (eval-bool-expression
  (make-my-or(make-my-and #false #true)
             (make-my-or #false #false)))#false)
(check-expect
 (eval-bool-expression
  (make-my-and(make-my-or #false #false)
              (make-my-or #false #true)))#false)
(check-expect
 (eval-bool-expression
  (make-my-and(make-my-and #true #false)
              (make-my-or #false #true)))#false)
(check-expect
 (eval-bool-expression
  (make-my-and(make-my-and #true #true)
              (make-my-or #false #true)))#true)
(check-expect
 (eval-bool-expression
  (make-my-and(make-my-and(make-my-not #true)#true)
              (make-my-or #false #true)))#false)
(check-expect
 (eval-bool-expression
  (make-my-and(make-my-not #false)
              (make-my-or #false #true)))#true)
(check-expect
 (eval-bool-expression
  (make-my-not #true))#false)
(check-expect
 (eval-bool-expression
  (make-my-not #false))#true)

(define(eval-bool-expression expression)
  (cond
    ((boolean? expression)expression)
    ((my-or? expression)
     (local((define left-evaluated(eval-bool-expression(my-or-left expression)))
            (define right-evaluated(eval-bool-expression(my-or-right expression))))
       (if left-evaluated #true right-evaluated)))
    ((my-and? expression)
     (local((define left-evaluated(eval-bool-expression(my-and-left expression)))
            (define right-evaluated(eval-bool-expression(my-and-right expression))))
       (if left-evaluated right-evaluated #false)))
    ((my-not? expression)
     (local((define expression-evaluated(eval-bool-expression(my-not-boolean expression))))
       (if expression-evaluated #false #true)))))

;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Parse Expression

(define WRONG "Something went wrong")

(define(atom? a)
  (or(number? a)
     (string? a)
     (symbol? a)))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(check-expect(parse 5)5)
(check-error(parse'(1 2 3)))
(check-error(parse'(z 2 3)))
(check-error(parse'(+ 1)))
(check-error(parse "asd"))
(check-error(parse'(+ 1 z)))

(check-expect(parse'(+ 3 4))(make-add 3 4))
(check-expect(parse'(* 3 4))(make-mul 3 4))
(check-expect(parse'(*(+ 1 2)4))(make-mul(make-add 1 2)4))
(check-expect(parse'(+(* 1 2)4))(make-add(make-mul 1 2)4))
(check-expect(interpreter-expr'(+ 2 3))5)
(check-expect(interpreter-expr'(* 2 3))6)
(check-expect(interpreter-expr'(+(* 7 3)(+ 2 4)))27)
(check-expect(interpreter-expr'(*(+ 7 3)(* 2 4)))80)
(check-expect(interpreter-expr'(+ 2 3))5)

(check-error(interpreter-expr'(* 2)))
(check-error(interpreter-expr'(* 2 'z)))
(check-error(interpreter-expr'(g 2 1)))


(define(interpreter-expr expression)
  (eval-expression(parse expression)))
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; 21.2 Interpreting Variables

(check-expect(subst 'z 's 12)'z)
(check-expect(subst 'z 'z 12)12)
(check-expect(subst 16 's 12)16)
(check-expect(subst(make-mul 'z 'z)'z 12)(make-mul 12 12))
(check-expect(subst(make-mul 's 'z)'z 12)(make-mul 's 12))
(check-expect
 (subst(make-mul(make-add 'z 'z)'z)'z 12)
 (make-mul(make-add 12 12)12))

(define(subst expression symbol number)
  (cond
    ((number? expression)expression)
    ((symbol? expression)
     (if(symbol=? expression symbol)number expression))
    ((mul? expression)
     (make-mul(subst(mul-left expression)symbol number)
              (subst(mul-right expression)symbol number)))
    ((add? expression)
     (make-add(subst(add-left expression)symbol number)
              (subst(add-right expression)symbol number)))
    ((application? expression)
     (make-application
      (application-name expression)
      (subst(application-argument expression)symbol number)))))

(define(numeric? expression)
  (cond
    ((number? expression)#true)
    ((add? expression)
     (and(numeric?(add-left expression))
         (numeric?(add-right expression))))
    ((mul? expression)
     (and(numeric?(mul-left expression))
         (numeric?(mul-right expression))))
    (else #false)))

(check-expect(numeric?(make-mul 2 3))#true)
(check-expect(numeric?(make-mul 'z 3))#false)
(check-expect(numeric?(make-mul(make-add 2 3)3))#true)
(check-expect(numeric?(make-mul(make-add 's 3)3))#false)

(check-error(eval-variable 'z))
(check-expect(eval-variable(make-add(make-add 2 3)(make-mul 1 2)))7)
(check-expect(eval-variable 2)2)
 
(define(eval-variable expression)
  (if(numeric? expression)
     (eval-expression expression)
     (error WRONG)))

(define(substall expression variable-defs)
  (cond
    ((empty? variable-defs)expression)
    (else(substall
          (subst expression
                 (first(first variable-defs))
                 (second(first variable-defs)))
          (rest variable-defs)))))

(define(eval-variable* expression variable-defs)
  (eval-variable(substall expression variable-defs)))
                
(check-expect
 (substall(make-add(make-mul 'a 'b)(make-add 'a 'a))'((a 2)(b 5)))
 (make-add(make-mul 2 5)(make-add 2 2)))
(check-expect
 (eval-variable*(make-add(make-mul 'a 'b)(make-add 'a 'a))'((a 2)(b 5)))14)
(check-error
 (eval-variable*(make-add(make-mul 'k 'b)(make-add 'a 'a))'((a 2)(b 5))))
     
(define(eval-var-lookup expression variable-defs)
  (cond
    ((number? expression)expression)
    ((symbol? expression)
     (local
       ((define assoc(assq expression variable-defs)))
       (if(boolean? assoc)
          (error "variable not defined")
          (second assoc))))
    ((add? expression)
     (+(eval-var-lookup
        (add-left expression)variable-defs)
       (eval-var-lookup
        (add-right expression)variable-defs)))
    ((mul? expression)
     (*(eval-var-lookup
        (mul-left expression)variable-defs)
       (eval-var-lookup
        (mul-right expression)variable-defs)))))

(check-expect
 (eval-var-lookup(make-add(make-mul 'a 'b)(make-add 'a 'a))'((a 2)(b 5)))14)
(check-error
 (eval-var-lookup(make-add(make-mul 'a 'r)(make-add 'a 'a))'((a 2)(b 5))))
(check-expect(eval-var-lookup 12'((a 2)(b 5)))12)
(check-expect(eval-var-lookup 'a '((a 2)(b 5)))2)
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; 21.3 Interpreting Functions

(define-struct application [name argument])
(define example1(make-application 'k (make-add 1 1)))
(define example2(make-mul 5(make-application 'k(make-mul 1 1))))
(define example3(make-mul(make-application 'i 5)(make-application 'k(make-mul 1 1))))

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; – (make-application BSL-fun-expr BSL-fun-expr)

;; If eval-definition1 finds function with name fname
;; it evaluates its argument, substitutes all occurrences
;; of fparam symbol with this argument in body and evaluates whole body
(define(eval-definition1 expression fname fparam fbody)
  (cond
    ((number? expression)expression)
    ((symbol? expression)(error WRONG))
    ((add? expression)
     (+(eval-definition1(add-left expression)fname fparam fbody)
       (eval-definition1(add-right expression)fname fparam fbody)))
    ((mul? expression)
     (*(eval-definition1(mul-left expression)fname fparam fbody)
       (eval-definition1(mul-right expression)fname fparam fbody)))
    ((application? expression)
     (if(symbol=?(application-name expression)fname)
        (local((define value(eval-definition1(application-argument expression)fname fparam fbody))
               (define plugd(subst fbody fparam value)))
          (eval-definition1 plugd fname fparam fbody))
        (error WRONG)))))

;; (k(+ 1 1))
(check-expect(eval-definition1 example1 'k 'r (make-add 'r 3))5)
;; (* 5(k(* 1 1)))
(check-expect(eval-definition1 example2 'k 'r (make-mul 'r 4))20)
;; wrong function name
(check-error(eval-definition1 example2 'a 'r (make-mul 'r 4)))
;; wrong parameter name
(check-error(eval-definition1 example2 'k 's (make-mul 'r 4)))


;; Exercise 358. Provide a structure type

;; function definition
(define-struct definition [name param body])
;; (define (f x) (+ 3 x))
(define function-definition1
  (make-definition 'f 'x(make-add 3 'x)))
;; (define (g y) (f (* 2 y)))
(define function-definition2
  (make-definition 'g 'y(make-application 'f(make-mul 2 'y))))
;; (define (h v) (+ (f v) (g v)))
(define function-definition3
  (make-definition 'h 'v(make-add(make-application 'f 'v)
                                 (make-application 'g 'v))))

(define da-fgh(list function-definition1 function-definition2 function-definition3))

(check-expect(lookup-def da-fgh 'g)function-definition2)
(check-error(lookup-def da-fgh 'o))

(define(lookup-def da f)
  (cond
    ((empty? da)(error "cant find function definition"))
    ((symbol=? f(definition-name(first da)))(first da))
    (else(lookup-def(rest da)f))))

;; Exercise 359. Design eval-function*

(define(eval-function* ex da)
  (cond
    ((number? ex)ex)
    ((symbol? ex)(error WRONG))
    ((add? ex)
     (+(eval-function*(add-left ex)da)
       (eval-function*(add-right ex)da)))
    ((mul? ex)
     (*(eval-function*(mul-left ex)da)
       (eval-function*(mul-right ex)da)))
    ((application? ex)
     (local
       ((define fundef(lookup-def da(application-name ex)))
        (define value(eval-function*(application-argument ex)da))
        (define plugd
          (subst(definition-body fundef)
                (definition-param fundef)
                value)))
       (eval-function* plugd da)))))

(check-expect
 (eval-function*(make-application 'f(make-add 3 4))da-fgh)10)
(check-expect
 (eval-function*(make-mul(make-application 'f(make-add 3 4))2)da-fgh)20)
;; (+(+ 4 3)(+(* 4 2)3))
(check-expect
 (eval-function*(make-application 'h 4)da-fgh)18)
(check-expect
 (eval-function*(make-application 'g 5)da-fgh)13)
(check-error
 (eval-function*(make-mul(make-application 'm(make-add 3 4))2)da-fgh))
(check-error(eval-function* 'f da-fgh))
(check-error
 (eval-function*(make-application 'h 'u)da-fgh))

;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; 21.4 Interpreting Everything

(define-struct con-definition[symbol value])

;; BSL-da-all is list of constant definitions and function definitions
;; - con-definition(constant definition)
;; - definition(function definition)
;; BSL-da-all is definition area

;; example definition area
(define example-BSL-da-all
  (list
   (make-con-definition 'close-to-pi 3.14)
   (make-con-definition 'two-pi(make-mul 2 'close-to-pi))
   (make-definition 'area-of-circle 'r
                    (make-mul 'close-to-pi(make-mul 'r 'r)))
   (make-definition 'volume-of-10-cylinder 'r
                    (make-mul 10(make-application 'area-of-circle 'r)))))
                               

;; find constant definition for symbol x
(define(lookup-con-def x BSL-da-all)
  (cond
    ((empty? BSL-da-all)(error "cant find constant definition"))
    ((definition?(first BSL-da-all))
     (lookup-con-def x(rest BSL-da-all)))
    ((con-definition?(first BSL-da-all))
     (if(symbol=?(con-definition-symbol(first BSL-da-all))x)
        (first BSL-da-all)
        (lookup-con-def x(rest BSL-da-all))))))

;; find function definition for symbol x
(define(lookup-fun-def f BSL-da-all)
  (cond
    ((empty? BSL-da-all)(error "cant find function definition"))
    ((definition?(first BSL-da-all))
     (if(symbol=?(definition-name(first BSL-da-all))f)
        (first BSL-da-all)
        (lookup-fun-def f(rest BSL-da-all))))
    ((con-definition?(first BSL-da-all))
     (lookup-fun-def f(rest BSL-da-all)))))

(check-expect
 (lookup-con-def 'close-to-pi example-BSL-da-all)
 (make-con-definition 'close-to-pi 3.14))
(check-error(lookup-con-def 'other-constant-def example-BSL-da-all))
(check-expect
 (lookup-fun-def 'volume-of-10-cylinder example-BSL-da-all)
 (make-definition 'volume-of-10-cylinder 'r
                  (make-mul 10(make-application 'area-of-circle 'r))))
(check-error(lookup-fun-def 'other-function-def example-BSL-da-all))
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Exercise 361. Design eval-all.
(check-expect(eval-all 'close-to-pi example-BSL-da-all)3.14)
(check-expect(eval-all 5 example-BSL-da-all)5)
(check-expect
 (eval-all(make-mul 'close-to-pi 'close-to-pi)example-BSL-da-all)
 (* 3.14 3.14))
(check-expect
 (eval-all(make-add 'close-to-pi 'close-to-pi)example-BSL-da-all)
 (+ 3.14 3.14))
(check-expect(eval-all 'two-pi example-BSL-da-all)(* 2 3.14))
(check-expect
 (eval-all(make-application 'area-of-circle 1)example-BSL-da-all)
 3.14)
(check-expect
 (eval-all(make-mul 3 'close-to-pi)example-BSL-da-all)9.42)
(check-expect
 (eval-all(make-application 'volume-of-10-cylinder 2)example-BSL-da-all)
 125.6)

;; eval-all
(define(eval-all exp BSL-da-all)
  (cond
    ((number? exp)exp)
    ((symbol? exp)
     (eval-all(con-definition-value(lookup-con-def exp BSL-da-all))BSL-da-all))
    ((mul? exp)(*(eval-all(mul-left exp)BSL-da-all)
                 (eval-all(mul-right exp)BSL-da-all)))
    ((add? exp)(+(eval-all(add-left exp)BSL-da-all)
                 (eval-all(add-right exp)BSL-da-all)))
    ((application? exp)
     (local
       ((define fun-def(lookup-fun-def(application-name exp)BSL-da-all))
        (define value(eval-all(application-argument exp)BSL-da-all))
        (define plugd
          (subst(definition-body fun-def)
                (definition-param fun-def)
                value)))
       (eval-all plugd BSL-da-all)))))

;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Exercise 362. It is cumbersome to enter

(define defs-to-parse
  '((a 12)
    (b 8)
    (c b)
    (addone r(+ r 1))
    (addtwo r(addone(addone r)))))

(check-expect
 (parse-defs defs-to-parse)
 (list
  (make-con-definition 'a 12)
  (make-con-definition 'b 8)
  (make-con-definition 'c 'b)
  (make-definition 'addone 'r(make-add 'r 1))
  (make-definition 'addtwo 'r
                   (make-application 'addone
                                     (make-application 'addone 'r)))))

(define(parse-exp exp)
  (cond
    ((or(number? exp)(symbol? exp))exp)
    (else
     (local((define L(length exp)))
       (cond
         ((= L 3)
          (cond
            ((symbol=?(first exp)'+)
             (make-add(parse-exp(second exp))
                      (parse-exp(third exp))))
            ((symbol=?(first exp)'*)
             (make-mul(parse-exp(second exp))
                      (parse-exp(third exp))))))
         ((= L 2)
          (make-application
           (if(symbol?(first exp))
              (first exp)
              (error WRONG))
           (parse-exp(second exp))))
         (else(error WRONG)))))))
       

(define(parse-defs defs)
  (cond
    ((empty? defs)'())
    (else
     (local
       ((define current-def(first defs))
        (define current-length(length current-def)))
       (cons
        (cond
          ((= current-length 2)
           (make-con-definition
            (if(symbol?(first current-def))
               (first current-def)
               (error WRONG))
            (parse-exp(second current-def))))
          ((= current-length 3)
           (if(and(symbol?(first current-def))
                  (symbol?(second current-def)))
              (make-definition
               (first current-def)
               (second current-def)
               (parse-exp(third current-def)))
              (error WRONG)))
          (else(error WRONG)))
        (parse-defs(rest defs)))))))

(define(interpreter exp defs)
  (eval-all
   (parse-exp exp)
   (parse-defs defs)))

(check-error(parse-defs'((z 6 7))))
(check-error(parse-defs'((1 1))))
(check-error(parse-defs'((z 1 2 3))))
(check-error(interpreter'(+ 2 2 3)defs-to-parse))
(check-error(interpreter'(+ 2 z)defs-to-parse))
(check-error(parse-exp'(3 4)))

(check-expect
 (parse-exp'(+ 4(u z)))
 (make-add 4 (make-application 'u 'z)))
(check-expect
 (interpreter'(+(* 5 2)a)defs-to-parse)
 (+(* 5 2)12))
(check-expect
 (interpreter'(addone 5)defs-to-parse)6)
(check-expect
 (interpreter'(addone a)defs-to-parse)13)
(check-expect
 (interpreter'(addtwo a)defs-to-parse)14)
(check-expect
 (interpreter'(addtwo(+(*(+(addtwo a)(addone b))2)7))defs-to-parse)
 (+(+(*(+(+ 2 12)(+ 1 8))2)7)2))


