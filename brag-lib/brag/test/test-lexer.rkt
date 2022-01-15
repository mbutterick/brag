#lang racket/base
(require brag/rules/lexer
         rackunit
         br-parser-tools/lex)

(define (l s)
  (define t (lex/1 (open-input-string s)))
  (list (token-name (position-token-token t))
        (token-value (position-token-token t))
        (position-offset (position-token-start-pos t))
        (position-offset (position-token-end-pos t))))

;; WARNING: the offsets are not in terms of file positions.  So they
;; start counting at 1, not 0.
(check-equal? (l " hi")
              '(ID "hi" 2 4))

(check-equal? (l "  hi")
              '(ID "hi" 3 5))

(check-equal? (l "hi")
              '(ID "hi" 1 3))

(check-equal? (l "# foobar\nhi")
              '(ID "hi" 10 12))

(check-equal? (l "# foobar\rhi")
              '(ID "hi" 10 12))

(check-equal? (l "# foobar\r\nhi")
              '(ID "hi" 11 13))

(check-equal? (l "hi:")
              '(RULE_HEAD "hi:" 1 4))

(check-equal? (l "hi   :")
              '(RULE_HEAD "hi   :" 1 7))

(check-equal? (l "|")
              '(PIPE "|" 1 2))

(check-equal? (l "(")
              '(LPAREN "(" 1 2))

(check-equal? (l "[")
              '(LBRACKET "[" 1 2))

(check-equal? (l ")")
              '(RPAREN ")" 1 2))

(check-equal? (l "]")
              '(RBRACKET "]" 1 2))

;; 220111: lexer now converts single-quoted lexemes
;; to standard Racket-style double-quoted string literal
(check-equal? (l "'hello'")
              '(LIT "\"hello\"" 1 8))

(check-equal? (l "'he\\'llo'")
              '(LIT "\"he'llo\"" 1 10))

(check-equal? (l "/")
              '(HIDE "/" 1 2))

(check-equal? (l " /")
              '(HIDE "/" 2 3))

(check-equal? (l "@")
              '(SPLICE "@" 1 2))

(check-equal? (l " @")
              '(SPLICE "@" 2 3))

(check-equal? (l "#:prefix-out val:")
              (list 'EOF eof 18 18)) ; lexer skips kwarg