#lang racket/base
(require brag/support (submod brag/rules/lexer lex-abbrevs) brag/support racket/match)
(provide color-brag)

(define brag-syntax-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [whitespace (return-without-srcloc (brag-syntax-lexer input-port))]
   [(:or (from/to "'" "'") (from/to "\"" "\"")) (token 'LIT lexeme)]
   [(:or (char-set "()[]|+*:") hide-char splice-char) (token 'MISC lexeme)]
   [(:seq (:or "#" ";")
          (complement (:seq (:* any-char) NL (:* any-char)))
          (:or NL ""))
    (token 'COMMENT lexeme)]
   [id (token 'ID lexeme)]
   [any-char (token 'OTHER lexeme)]))


(define (color-brag port)
  (define srcloc-tok
    (with-handlers
        ([exn:fail:read?
          (λ (exn) (srcloc-token (token 'ERROR) (car (exn:fail:read-srclocs exn))))])
      (brag-syntax-lexer port)))
  (if (eof-object? srcloc-tok)
      (values srcloc-tok 'eof #f #f #f)
      (match-let* ([(srcloc-token
                     (token-struct type val _ _ _ _ _)
                     (srcloc _ _ _ posn span)) srcloc-tok]
                   [(cons start end) (cons posn (+ posn span))]
                   [(cons _ cat) (or (assq type
                                           '((ID . symbol)
                                             (LIT . string)
                                             (MISC . parenthesis)
                                             (COMMENT . comment)
                                             (ERROR . error)))
                                     (cons 'OTHER 'no-color))])
        (values val cat #f start end))))

(module+ test
  (require rackunit sugar/list)
  (define (apply-colorer str)
    (for/list ([annotation (in-port (λ (p)
                                      (let ([xs (values->list (color-brag p))])
                                        (if (eof-object? (car xs)) eof xs)))
                                    (open-input-string str))])
      annotation))

  (check-equal? (apply-colorer "foo") '(("foo" symbol #f 1 4)))
  (check-equal? (apply-colorer "'str'") '(("'str'" string #f 1 6)))
  (check-equal? (apply-colorer "(foo)+") '(("(" parenthesis #f 1 2)
                                           ("foo" symbol #f 2 5)
                                           (")" parenthesis #f 5 6)
                                           ("+" parenthesis #f 6 7)))
  (check-equal? (apply-colorer "; rem") '(("; rem" comment #f 1 6)))
  (check-equal? (apply-colorer "◊") '(("◊" no-color #f 1 4))))
