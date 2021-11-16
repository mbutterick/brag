#lang racket/base

(require rackunit
         brag/support
         br-parser-tools/lex
         brag/rules/parser
         brag/rules/lexer
         )

(require "make-rule-parser-grammar.rkt")


(define parse-next
  (make-rule-parser next))

(define parse-start
  (make-rule-parser start))

(define (lex ip)
  (port-count-lines! ip)
  (lambda ()
    (define next-char (read-char ip))
    (cond [(eof-object? next-char)
           (token eof)]
          [(char=? next-char #\0)
           (token "0" "0")]
          [(char=? next-char #\1)
           (token "1" "1")])))


(check-equal? (syntax->datum (parse #f (lex (open-input-string "0"))))
              '(start (next "0")))
(check-equal? (syntax->datum (parse #f (lex (open-input-string "0"))))
              (syntax->datum (parse (lex (open-input-string "0")))))

(check-equal? (syntax->datum (parse-start #f (lex (open-input-string "0"))))
              '(start (next "0")))
(check-equal? (syntax->datum (parse-start #f (lex (open-input-string "0"))))
              (syntax->datum (parse-start (lex (open-input-string "0")))))

(check-equal? (syntax->datum (parse-next #f (lex (open-input-string "0"))))
              '(next "0"))
(check-equal? (syntax->datum (parse-next #f (lex (open-input-string "0"))))
              (syntax->datum (parse-next (lex (open-input-string "0")))))