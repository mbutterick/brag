#lang racket/base
(require brag/support (submod brag/rules/lexer lex-abbrevs) racket/match)
(provide color-brag)

(define brag-syntax-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   ;; need to lex whitespace to keep srclocs accurate
   ;; (for DrRacket selections etc)
   [whitespace (token 'WHITE lexeme)]
   [(:or (from/to "'" "'") (from/to "\"" "\"")) (token 'LIT lexeme)]
   [(:or (char-set "()[]|+*:") hide-char splice-char) (token 'MISC lexeme)]
   [(:seq (:or "#" ";") (complement (:seq (:* any-char) NL (:* any-char))) (:or NL "")) (token 'COMMENT lexeme)]
   [id (token 'ID lexeme)]
   [any-char (token 'OTHER lexeme)]))

(define default-backup 10)
(define (color-brag port [backup default-backup] [in-string? #f])
  (define srcloc-tok (brag-syntax-lexer port))
  (if (eof-object? srcloc-tok)
      (values srcloc-tok 'eof #f #f #f 0 #f)
      (match-let* ([(srcloc-token (token-struct type val _ _ _ _ _) (srcloc _ _ _ posn span)) srcloc-tok]
                   [(cons start end) (cons posn (+ posn span))]
                   [(cons _ cat) (or (assq type
                                           '((ID . symbol)
                                             (LIT . string)
                                             (MISC . parenthesis)
                                             (WHITE . whitespace)
                                             (COMMENT . comment)))
                                     (cons 'OTHER 'no-color))])
        (values val cat #f start end backup #f))))

(module+ test
  (require rackunit)
  (define-syntax-rule (values->list EXPR) (call-with-values (λ () EXPR) list))
  (define (apply-colorer str)
    (for/list ([annotation (in-port (λ (p)
                                      (let ([xs (values->list (color-brag p))])
                                        (if (eof-object? (car xs)) eof xs)))
                                    (open-input-string str))])
      annotation))

  (check-equal? (apply-colorer "foo") `(("foo" symbol #f 1 4 ,default-backup #f)))
  (check-equal? (apply-colorer "'str'") `(("'str'" string #f 1 6 ,default-backup #f)))
  (check-equal? (apply-colorer "(foo)+") `(("(" parenthesis #f 1 2 ,default-backup #f)
                                           ("foo" symbol #f 2 5 ,default-backup #f)
                                           (")" parenthesis #f 5 6 ,default-backup #f)
                                           ("+" parenthesis #f 6 7 ,default-backup #f)))
  (check-equal? (apply-colorer "; rem") `(("; rem" comment #f 1 6 ,default-backup #f)))
  (check-equal? (apply-colorer "◊") `(("◊" no-color #f 1 4 ,default-backup #f))))
