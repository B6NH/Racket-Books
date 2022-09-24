; Exercise 11

#lang racket

; (match '(from me to you) '(from me to you)) - 5
; (match '(*x *y *x) '(a b c a b)) - 15
; (match '(*x *y *z) '(a b c a b)) - 4
; (match '(*x hey *y bulldog *z) '(a hey b bulldog c)) - 12
; (match '(*x a b c d e f) '(a b c d e f)) - 14
; (match '(a b c d e f *x) '(a b c d e f)) - 8

; It is easier to match pattern without placeholders or
; when all placeholders are unique and appear at the end

#t
