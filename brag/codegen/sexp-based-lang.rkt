#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     "codegen.rkt"
                     "flatten.rkt")
         br-parser-tools/lex
         br-parser-tools/cfg-parser
         (prefix-in bs: brag/support)
         racket/set)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [brag-module-begin #%module-begin]
                     [bs:apply-lexer apply-lexer] ; for repl
                     [bs:apply-tokenizer-maker apply-tokenizer-maker])) ; for repl

;; -> (listof symbol)
(define-for-syntax (rules->token-types rules)
  (let-values ([(implicit-tokens explicit-tokens) (rules-collect-token-types rules)])
    (remove-duplicates (append (for/list ([it (in-list implicit-tokens)])
                                 (string->symbol (syntax-e it)))
                               (map syntax-e explicit-tokens)) eq?)))

(define-syntax (brag-module-begin rules-stx)
  (syntax-case rules-stx ()
    [(_) (raise-syntax-error 'brag
                             (format "The grammar does not appear to have any rules")
                             'brag-module)]
    [(_ . RULES)
     (let ([rules (syntax->list #'RULES)]) ;; (listof stx)
       
       (check-all-rules-defined! rules)
       (check-all-rules-no-duplicates! rules)
       (check-all-rules-satisfiable! rules)
       
       (define rule-ids (map rule-id rules))
       
       (with-syntax ([START-ID (first rule-ids)] ; The first rule, by default, is the start rule.
                     [((TOKEN-TYPE . TOKEN-TYPE-CONSTRUCTOR) ...)
                      (for/list ([tt (in-list (rules->token-types rules))])
                        (cons tt (string->symbol (format "token-~a" tt))))]
                     ;; Flatten rules to use the yacc-style ruleset that br-parser-tools supports       
                     [GENERATED-RULE-CODES (map flat-rule->yacc-rule (flatten-rules rules))]
                     ;; main exports. Break hygiene so they're also available at top-level / repl
                     [(PARSE PARSE-TO-DATUM PARSE-TREE MAKE-RULE-PARSER ALL-TOKEN-TYPES)
                      (map (λ (sym) (datum->syntax rules-stx sym))
                           '(parse parse-to-datum parse-tree make-rule-parser all-token-types))]
                     [TOKEN (datum->syntax rules-stx 'token)] ; for repl
                     [RULE-IDS (map syntax-e rule-ids)]
                     [RULES-STX rules-stx])
         ;; this stx object represents the top level of a #lang brag module.
         ;; so any `define`s are automatically available at the repl.
         ;; and only identifiers explicitly `provide`d are visible on import.
         #'(#%module-begin             
            (provide PARSE PARSE-TO-DATUM PARSE-TREE MAKE-RULE-PARSER ALL-TOKEN-TYPES)

            ;; handle brag/support `token` with special identifier
            ;; so it doesn't conflict with brag's internal `token` macro 
            (define TOKEN bs:token)
             
            (define-tokens enumerated-tokens (TOKEN-TYPE ...))
             
            ;; all-token-types lists all the tokens (except for EOF)
            (define ALL-TOKEN-TYPES (set-remove (set 'TOKEN-TYPE ...) 'EOF))
             
            ;; For internal use by the permissive tokenizer only:
            (define all-tokens-hash/mutable
              (make-hash (list ;; Note: we also allow the eof object here, to make
                          ;; the permissive tokenizer even nicer to work with.
                          (cons eof token-EOF) 
                          (cons 'TOKEN-TYPE TOKEN-TYPE-CONSTRUCTOR) ...)))
             
            (define-syntax (MAKE-RULE-PARSER rule-id-stx)
              (syntax-case rule-id-stx ()
                [(_ start-rule)
                 (and (identifier? #'start-rule)
                      (member (syntax-e #'start-rule) 'RULE-IDS))
                 ;; The cfg-parser depends on the start-rule provided in (start ...) to have the same
                 ;; context as the rest of this body. Hence RECOLORED-START-RULE
                 (with-syntax ([RECOLORED-START-RULE (datum->syntax #'RULES-STX (syntax-e #'start-rule))])
                   #'(let ([THE-GRAMMAR (cfg-parser (tokens enumerated-tokens)
                                                    (src-pos)
                                                    (start RECOLORED-START-RULE)
                                                    (end EOF)
                                                    (error THE-ERROR-HANDLER)
                                                    (grammar . GENERATED-RULE-CODES))])
                       (procedure-rename
                        (case-lambda [(tokenizer)
                                      (define next-token
                                        (make-permissive-tokenizer tokenizer all-tokens-hash/mutable))
                                      (THE-GRAMMAR next-token)]
                                     [(source tokenizer)
                                      (parameterize ([current-source source])
                                        (PARSE tokenizer))])
                        (string->symbol (format "~a-rule-parser" 'start-rule)))))]
                [(_ not-a-rule-id)
                 (raise-syntax-error #f
                                     (format "Rule ~a is not defined in the grammar" (syntax-e #'not-a-rule-id))
                                     rule-id-stx)]))

            ;; start-id has to be a value, not an expr, because make-rule-parser is a macro
            (define PARSE (procedure-rename (MAKE-RULE-PARSER START-ID) 'PARSE))
             
            (define (PARSE-TO-DATUM x)
              (let loop ([x (syntax->datum (PARSE x))])
                (cond
                  [(list? x) (map loop x)]
                  [else x])))

            (define PARSE-TREE PARSE-TO-DATUM))))]))

