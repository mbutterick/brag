#lang racket/base
(require brag/support (submod brag/rules/lexer lex-abbrevs) racket/match)
(provide color-brag)

(define brag-syntax-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   ;; need to lex whitespace to keep srclocs accurate
   ;; (for DrRacket selections etc)
   [whitespace (token 'WHITE lexeme)]
   [(:or (:: "\"" "\\" "\"" "\"") ; string containg double-quote = "\""
         (from/to "'" "'")
         (from/to "\"" "\"")) (token 'LIT lexeme)]
   [(:or "()" "Ø" "∅") (token 'NO-COLOR lexeme)] ; empty set symbols
   [(:or (char-set "()[]{}|+*:?") hide-char splice-char "::=") (token 'MISC lexeme)]
   [(from/to "(*" "*)") (token 'COMMENT lexeme)]
   [(:seq (:or "#" ";") (complement (:seq (:* any-char) NL (:* any-char))) (:or NL "")) (token 'COMMENT lexeme)]
   [id (token 'ID lexeme)]
   [any-char (token 'OTHER lexeme)]))

(define (color-brag port)
  (define srcloc-tok (brag-syntax-lexer port))
  (cond
    [(eof-object? srcloc-tok) (values srcloc-tok 'eof #f #f #f)]
    [else
     (match-define (srcloc-token (token-struct type val _ _ _ _ _) (srcloc _ _ _ posn span)) srcloc-tok)
     (match-define (list start end) (list posn (+ posn span)))
     (values val (case type
                   [(ID) 'symbol]
                   [(LIT) 'string]
                   [(MISC) 'parenthesis]
                   [(WHITE) 'whitespace]
                   [(COMMENT) 'comment]
                   [else 'no-color]) #f start end)]))

(module+ test
  (require rackunit)
  (define-syntax-rule (values->list EXPR) (call-with-values (λ () EXPR) list))
  (define (apply-colorer str)
    (for/list ([annotation (in-port (λ (p)
                                      (let ([xs (values->list (color-brag p))])
                                        (if (eof-object? (car xs)) eof xs)))
                                    (open-input-string str))])
      annotation))

  (check-equal? (apply-colorer "foo") `(("foo" symbol #f 1 4)))
  (check-equal? (apply-colorer "'str'") `(("'str'" string #f 1 6)))
  (check-equal? (apply-colorer "(foo)+") `(("(" parenthesis #f 1 2)
                                           ("foo" symbol #f 2 5)
                                           (")" parenthesis #f 5 6)
                                           ("+" parenthesis #f 6 7)))
  (check-equal? (apply-colorer "; rem") `(("; rem" comment #f 1 6)))
  (check-equal? (apply-colorer "◊") `(("◊" no-color #f 1 4))))
