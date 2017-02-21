#lang racket/base

(provide tokenize)

;; A simple lexer for simple-line-drawing.
(require brag/support
         br-parser-tools/lex)

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos 
     [(repetition 1 +inf.0 numeric)
      (token 'INTEGER (string->number lexeme))]
     [upper-case
      (token 'STRING lexeme)]
     ["b"
      (token 'STRING " ")]
     [";"
      (token ";" lexeme)]
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof)
      (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

