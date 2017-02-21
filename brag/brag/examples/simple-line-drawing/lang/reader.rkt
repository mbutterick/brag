#lang s-exp syntax/module-reader
brag/examples/simple-line-drawing/semantics
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
#:whole-body-readers? #t

(require brag/examples/simple-line-drawing/lexer
         brag/examples/simple-line-drawing/grammar)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src ip)
  (list (parse src (tokenize ip))))

(define (my-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/default-lexer 'default-lexer)]
    [else
     (default-filter key default)]))
