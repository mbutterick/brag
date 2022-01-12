#lang debug at-exp racket/base
(require (for-syntax racket/base "parser.rkt"))
(require br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         "parser.rkt"
         "rule-structs.rkt"
         (only-in brag/support from/to)
         racket/string)

(provide lex/1 tokenize)
(module+ lex-abbrevs
  (provide hide-char splice-char id-char letter digit NL id))

;; A newline can be any one of the following.
(define-lex-abbrev NL (:or "\r\n" "\r" "\n"))

;; reserved-chars = chars used for quantifiers & parse-tree filtering
(define-for-syntax quantifiers "+:*?{}") ; colon is reserved to separate rules and productions
(define-lex-trans reserved-chars
  (λ(stx) #`(char-set #,(format "~a~a~a" quantifiers hide-char splice-char))))

(define-lex-trans hide-char-trans (λ(stx) #`(char-set #,(format "~a" hide-char))))
(define-lex-trans splice-char-trans (λ(stx) #`(char-set #,(format "~a" splice-char))))

(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)]
  [id-char (:or letter digit (:& (char-set "+:*@!-.$%&/=?^_~<>") (char-complement (reserved-chars))))]
  [hide-char (hide-char-trans)]
  [splice-char (splice-char-trans)]
  )

(define-lex-abbrev id (:& (complement (:+ digit)) (:+ id-char)))
(define-lex-abbrev id-separator (:or ":" "::="))

(define-lex-abbrev esc-chars (union "\\a" "\\b" "\\t" "\\n" "\\v" "\\f" "\\r" "\\e"))

(require syntax-color/racket-lexer)

(define (unescape-double-quoted-lexeme lexeme)
  (list->string `(#\" ,@(string->list (read (open-input-string lexeme))) #\")))

(define (convert-to-double-quoted lexeme)
  (define outside-quotes-removed (string-trim lexeme "'"))
  (define single-quotes-unescaped (string-replace outside-quotes-removed "\\'" "'"))
  (define double-quotes-escaped (string-replace single-quotes-unescaped "\"" "\\\""))
  (define double-quotes-on-ends (string-append "\"" double-quotes-escaped "\""))
  double-quotes-on-ends)

(define lex/1
  (lexer-src-pos
   ;; handle whitespace & escape chars within quotes as literal tokens: "\n" "\t" '\n' '\t'
   ;; match the escaped version, and then unescape them before they become token-LITs
   [(:or (:: "'\\\\'") ; aka '\\'
         (:: "'" (:* (:or "\\'" esc-chars (:~ "'" "\\"))) "'"))
    (token-LIT (unescape-double-quoted-lexeme (convert-to-double-quoted lexeme)))]
   [(:or (:: "\"\\\\\"") ; aka "\\"
         (:: "\"" (:* (:or "\\\"" esc-chars (:~ "\"" "\\"))) "\""))
    (token-LIT (unescape-double-quoted-lexeme lexeme))]
   [(:or "()" "Ø" "∅") (token-EMPTY lexeme)]
   ["("
    (token-LPAREN lexeme)]
   ["["
    (token-LBRACKET lexeme)]
   [")"
    (token-RPAREN lexeme)]
   ["]"
    (token-RBRACKET lexeme)]
   [hide-char
    (token-HIDE lexeme)]
   [splice-char
    (token-SPLICE lexeme)]
   ["|"
    (token-PIPE lexeme)]
   [(:or "+" "*" "?"
         (:: "{" (:* digit) (:? (:: "," (:* digit))) "}"))
    (token-REPEAT lexeme)]
   ;; Skip whitespace
   [whitespace
    (return-without-pos (lex/1 input-port))]
   ;; skip multiline comments
   [(from/to "(*" "*)") (return-without-pos (lex/1 input-port))]
   ;; Skip comments up to end of line
   [(:: (:or "#" ";")
        (complement (:: (:* any-char) NL (:* any-char)))
        (:or NL ""))
    (return-without-pos (lex/1 input-port))]
   ;; skip commas (concatenation is implied)
   ["," (return-without-pos (lex/1 input-port))]
   [(eof)
    (token-EOF lexeme)]
   [(:: id (:* whitespace) id-separator)
    (token-RULE_HEAD lexeme)]
   [(:: hide-char id (:* whitespace) id-separator)
    (token-RULE_HEAD_HIDDEN lexeme)]
   [(:: splice-char id (:* whitespace) id-separator)
    (token-RULE_HEAD_SPLICED lexeme)]
   [id
    (token-ID lexeme)]
   
   ;; We call the error handler for everything else:
   [(:: any-char)
    (let-values ([(rest-of-text end-pos-2)
                  (lex-nonwhitespace input-port)])
      ((current-parser-error-handler)
       #f
       'error
       (string-append lexeme rest-of-text)
       (position->pos start-pos)
       (position->pos end-pos-2)))]))


;; This is the helper for the error production.
(define lex-nonwhitespace
  (lexer
   [(:+ (char-complement whitespace))
    (values lexeme end-pos)]
   [any-char
    (values lexeme end-pos)]
   [(eof)
    (values "" end-pos)]))



;; position->pos: position -> pos
;; Converts position structures from br-parser-tools/lex to our own pos structures.
(define (position->pos a-pos)
  (pos (position-offset a-pos)
       (position-line a-pos)
       (position-col a-pos)))



;; tokenize: input-port -> (-> token)
(define (tokenize ip #:source [source (object-name ip)])
  (λ () (parameterize ([file-path source])
          (lex/1 ip))))
