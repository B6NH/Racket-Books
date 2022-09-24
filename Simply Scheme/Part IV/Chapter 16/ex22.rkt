; Exercise 22

#lang racket

; For example 'Featherpad' allows to search using regular expressions
; To use this function, one must select option in lower right corner

; Some possible patterns:
; 'virtue' - explicit pattern
; 'zone|nerve' - 'zone' or 'nerve'
; 'enth[ue]siasm' - 'enthusiasm' or 'enthesiasm'
; 'whi?te' - 'white' or 'whte'
; 'emo*tion' - 'emtion', 'emotion', 'emootion', ...
; 'mor+al' - 'moral', 'morral', 'morrral', ...
; 'sce.ne' - 'sceane', 'scebne', 'scecne', ...
; '^file' - 'fileabc', 'filedef', 'file...', ...
; 'scale$' - 'abcscale', 'defscale', '...scale', ...
; '\d' - '0', '1', '2', ...

#t
