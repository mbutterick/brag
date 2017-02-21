#lang racket/base

;; Make sure the old token type also works fine.

(require brag/examples/simple-line-drawing
         brag/support
         racket/list
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         rackunit)

(define-tokens tokens (INTEGER STRING |;| EOF))

(define (make-tokenizer ip)
  (port-count-lines! ip)
  (define lex (lexer-src-pos 
               [(:+ numeric)
                (token-INTEGER (string->number lexeme))]
               [upper-case
                (token-STRING lexeme)]
               ["b"
                (token-STRING " ")]
               [";"
                (|token-;| lexeme)]
               [whitespace
                (return-without-pos (lex input-port))]
               [(eof)
                (token-EOF 'eof)]))
  (lambda ()
    (lex ip)))



(define the-parsed-object-stx
  (parse (make-tokenizer (open-input-string #<<EOF
3 9 X;
6 3 b 3 X 3 b;
3 9 X;
EOF
))))

(check-true (syntax-original? the-parsed-object-stx))
;; Does the rule name "drawing" also have the proper "original?" property set?
(check-true (syntax-original? (first (syntax->list the-parsed-object-stx))))

(check-equal? (syntax->datum the-parsed-object-stx)
              '(drawing (rows (repeat 3) (chunk 9 "X") ";")
                        (rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") ";")
                        (rows (repeat 3) (chunk 9 "X") ";")))

(define the-parsed-object (syntax->list the-parsed-object-stx))

(check-equal? (syntax-line the-parsed-object-stx) 1)
(check-equal? (syntax-column the-parsed-object-stx) 0)
(check-equal? (syntax-position the-parsed-object-stx) 1)
(check-equal? (syntax-span the-parsed-object-stx) 28)

(check-equal? (length the-parsed-object) 4)

(check-equal? (syntax->datum (second the-parsed-object))
              '(rows (repeat 3) (chunk 9 "X") ";"))
(check-equal? (syntax-line (list-ref the-parsed-object 1)) 1)

(check-equal? (syntax->datum (third the-parsed-object))
              '(rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") ";"))
(check-equal? (syntax-line (list-ref the-parsed-object 2)) 2)

(check-equal? (syntax->datum (fourth the-parsed-object))
              '(rows (repeat 3) (chunk 9 "X") ";"))
(check-equal? (syntax-line (list-ref the-parsed-object 3)) 3)

;; FIXME: add tests to make sure location is as we expect.
;;
;; FIXME: handle the EOF issue better.  Something in cfg-parser
;; appears to deviate from br-parser-tools/yacc with regards to the stop
;; token.
