#lang debug racket/base
(require (for-syntax racket/base "parser.rkt"))
(require br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         "parser.rkt"
         "rule-structs.rkt"
         (only-in brag/support from/to)
         racket/string
         syntax-color/racket-lexer)

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

(define (unescape-double-quoted-lexeme lexeme start-pos end-pos)
  ;; use `read` so brag strings have all the notational semantics of Racket strings
  (with-handlers ([exn:fail:read?
                   (λ (e) ((current-parser-error-handler)
                           #f
                           'error
                           lexeme
                           (position->pos start-pos)
                           (position->pos end-pos)))])
    (list->string `(#\" ,@(string->list (read (open-input-string lexeme))) #\"))))

(define (convert-to-double-quoted lexeme)
  ;; brag supports single-quoted strings, for some reason
  ;; (Racket does not. A single quote denotes a datum)
  ;; let's convert a single-quoted string into standard double-quoted style
  ;; so we can use Racket's `read` function on it.
  ;; and thereby support all the standard Racket string elements:
  ;; https://docs.racket-lang.org/reference/reader.html#%28part._parse-string%29
  (define outside-quotes-removed (string-trim lexeme "'"))
  (define single-quotes-unescaped (string-replace outside-quotes-removed "\\'" "'"))
  (define double-quotes-escaped (string-replace single-quotes-unescaped "\"" "\\\""))
  (define double-quotes-on-ends (string-append "\"" double-quotes-escaped "\""))
  double-quotes-on-ends)

(define-lex-abbrev backslash "\\")
(define-lex-abbrev single-quote "'")
(define-lex-abbrev escaped-single-quote (:: backslash single-quote))
(define-lex-abbrev double-quote "\"")
(define-lex-abbrev escaped-double-quote (:: backslash double-quote))
(define-lex-abbrev escaped-backslash (:: backslash backslash))

(define brag-lex
  (lexer-src-pos
   ;; we are delegating lexing of double-quoted strings to the Racket lexer (see below)
   #;[(:: double-quote ;; start with double quote
        (intersection ;; two conditions need to be true inside the quotes:
         ;; we can have anything except
         ;; a plain double-quote (which would close the quote)
         ;; plus we specially allow escaped double quotes and backslashes
         (:* (:or escaped-double-quote escaped-backslash (:~ double-quote)))
         ;; we must forbid one situation with the string \\"
         ;; the problem is that it's ambiguous:
         ;; it can be lexed as (:: escaped-backlash double-quote) = \\ + "
         ;; or  (:: backlash escaped-double-quote) = \ + \"
         ;; because escapes should be "left associative",
         ;; we forbid the second possibility
         ;; There are still some weird corner cases but the current tests work.
         ;; with single and double quotes in the mix,
         ;; I'm not sure how much better this can be.
         (complement (:: any-string backslash escaped-double-quote any-string)))
        double-quote) ;; end with double quote
    (token-LIT (unescape-double-quoted-lexeme lexeme start-pos end-pos))]
   ;; single-quoted string follows the same pattern,
   ;; but with escaped-single-quote instead of escaped-double-quote
   [(:: single-quote
        (intersection
         (:* (:or escaped-single-quote escaped-backslash (:~ single-quote)))
         (complement (:: any-string backslash escaped-single-quote any-string)))
        single-quote)
    (token-LIT (unescape-double-quoted-lexeme (convert-to-double-quoted lexeme) start-pos end-pos))]
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

(define (lex/1 ip)
  (cond
    [(equal? (peek-bytes 1 0 ip) #"\"")
     ;; delegate lexing of strings to the default Racket lexer
     ;; this doesn't yet work with single-quoted strings
     ;; but maybe there's a way to do that by messing with the readtable
     (define-values (line-start col-start pos-start) (port-next-location ip))
     (define-values (str type paren also-pos-start also-pos-end) (racket-lexer ip))
     (define-values (line-end col-end pos-end) (port-next-location ip))
     (make-position-token (token-LIT (string-append "\""
                                                    (read (open-input-string str))
                                                    "\""))
                          (make-position pos-start line-start col-start)
                          (make-position pos-end line-end col-end))]
    [else (brag-lex ip)]))

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
