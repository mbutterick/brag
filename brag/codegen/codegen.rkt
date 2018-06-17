#lang racket/base

(require (for-template racket/base)
         racket/list
         racket/set
         racket/syntax
         syntax/srcloc
         brag/rules/stx-types
         "flatten.rkt"
         syntax/id-table
         (prefix-in sat: "satisfaction.rkt")
         (prefix-in support: brag/support)
         (prefix-in stxparse: syntax/parse))

(provide rules-codegen)


;; Generates the body of the module.
;; FIXME: abstract this so we can just call (rules ...) without
;; generating the whole module body.
(define (rules-codegen rules-stx 
                       #:parser-provider-module [parser-provider-module 'br-parser-tools/yacc]
                       #:parser-provider-form [parser-provider-form 'parser])
  (syntax-case rules-stx ()
    [(_ RULE ...)
     (begin
       ;; (listof stx)
       (define rules (syntax->list #'(RULE ...)))
       
       (when (empty? rules)
         (raise-syntax-error 'brag
                             (format "The grammar does not appear to have any rules")
                             rules-stx))
       
       (check-all-rules-defined! rules)
       (check-all-rules-no-duplicates! rules)
       (check-all-rules-satisfiable! rules)
       
       ;; We flatten the rules so we can use the yacc-style ruleset that br-parser-tools
       ;; supports.
       (define flattened-rules (flatten-rules rules))
       
       (define generated-rule-codes (map flat-rule->yacc-rule flattened-rules))
       
       ;; The first rule, by default, is the start rule.
       (define rule-ids (for/list ([a-rule (in-list rules)])
                          (rule-id a-rule)))
       (define start-id (first rule-ids))
       
       
       (define-values (implicit-tokens    ;; (listof identifier)
                       explicit-tokens)   ;; (listof identifier)
         (rules-collect-token-types rules))
       
       ;; (listof symbol)
       (define implicit-token-types
         (map string->symbol
              (set->list (list->set (map syntax-e implicit-tokens)))))
       
       ;; (listof symbol)
       (define explicit-token-types
         (set->list (list->set (map syntax-e explicit-tokens))))
       
       ;; (listof symbol)
       (define token-types
         (set->list (list->set (append (map (lambda (x) (string->symbol (syntax-e x)))
                                            implicit-tokens)
                                       (map syntax-e explicit-tokens)))))
       
       (with-syntax ([start-id start-id]
                     
                     [(token-type ...) token-types]
                     
                     [(token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           token-types)]
                     
                     [(explicit-token-types ...) explicit-token-types]
                     [(implicit-token-types ...) implicit-token-types]
                     [(implicit-token-types-str ...) (map symbol->string implicit-token-types)]
                     [(implicit-token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           implicit-token-types)]
                     [generated-grammar #`(grammar #,@generated-rule-codes)]
                     [parser-module parser-provider-module]
                     [parser-form parser-provider-form]
                     [PARSE (syntax-local-introduce (or (syntax-property rules-stx 'parse) (error 'no-parse-id-prop)))]
                     [PARSE-TO-DATUM (syntax-local-introduce (or (syntax-property rules-stx 'parse-to-datum) (error 'no-parse-to-datum-id-prop)))]
                     [PARSE-TREE (syntax-local-introduce (or (syntax-property rules-stx 'parse-tree) (error 'no-parse-tree-id-prop)))]
                     [MAKE-RULE-PARSER (syntax-local-introduce (or (syntax-property rules-stx 'make-rule-parser) (error 'no-make-rule-parser-id-prop)))]
                     [ALL-TOKEN-TYPES (syntax-local-introduce (or (syntax-property rules-stx 'all-token-types) (error 'no-all-token-types-id-prop)))]
                     [TOKEN (syntax-local-introduce (or (syntax-property rules-stx 'token) (error 'no-token-id-prop)))]
                     [APPLY-LEXER (syntax-local-introduce (or (syntax-property rules-stx 'apply-lexer) (error 'no-apply-lexer-id-prop)))]
                     [APPLY-TOKENIZER-MAKER (syntax-local-introduce (or (syntax-property rules-stx 'apply-tokenizer-maker) (error 'no-apply-tokenizer-maker-id-prop)))])
         ;; this stx object represents the top level of a #lang brag module.
         ;; so any `define`s are automatically available at the repl.
         ;; and only identifiers explicitly `provide`d are visible on import.
         (quasisyntax/loc rules-stx
           (begin             
             (require br-parser-tools/lex
                      parser-module
                      brag/codegen/runtime
                      brag/support
                      brag/private/internal-support
                      racket/set
                      (for-syntax syntax/parse racket/base))
             
             (provide PARSE
                      PARSE-TO-DATUM
                      PARSE-TREE
                      MAKE-RULE-PARSER
                      ALL-TOKEN-TYPES
                      #;current-source
                      #;current-parser-error-handler
                      #;current-tokenizer-error-handler
                      #;[struct-out exn:fail:parsing]
                      )
             
             ;; helpers from brag/support
             (define TOKEN token)
             (define APPLY-LEXER apply-lexer)
             (define APPLY-TOKENIZER-MAKER apply-tokenizer-maker)
             
             (define-tokens enumerated-tokens (token-type ...))
             
             ;; all-token-types lists all the tokens (except for EOF)
             (define ALL-TOKEN-TYPES 
               (set-remove (set 'token-type ...) 'EOF))
             
             ;; For internal use by the permissive tokenizer only:
             (define all-tokens-hash/mutable
               (make-hash (list ;; Note: we also allow the eof object here, to make
                           ;; the permissive tokenizer even nicer to work with.
                           (cons eof token-EOF) 
                           (cons 'token-type token-type-constructor) ...)))
             
             
             #;(define default-lex/1
                 (lexer-src-pos [implicit-token-types-str
                                 (token 'implicit-token-types lexeme)]
                                ...
                                [(eof) (token eof)]))
             
             (define-syntax (MAKE-RULE-PARSER stx-2)
               (syntax-parse stx-2
                 [(_ start-rule:id)
                  (begin
                    ;; HACK HACK HACK
                    ;; The cfg-parser depends on the start-rule provided in (start ...) to have the same
                    ;; context as the rest of this body, so I need to hack this.  I don't like this, but
                    ;; I don't know what else to do.  Hence recolored-start-rule.
                    (unless (member (syntax-e #'start-rule)
                                    '#,(map syntax-e rule-ids))
                      (raise-syntax-error #f
                                          (format "Rule ~a is not defined in the grammar" (syntax-e #'start-rule))
                                          stx-2))
                    
                    (define recolored-start-rule (datum->syntax (syntax #,rules-stx) (syntax-e #'start-rule)))
                    #`(let ([THE-GRAMMAR (parser-form (tokens enumerated-tokens)
                                                      (src-pos)
                                                      (start #,recolored-start-rule)
                                                      (end EOF)
                                                      (error THE-ERROR-HANDLER)
                                                      generated-grammar)])
                        (procedure-rename
                         (case-lambda [(tokenizer)
                                       (define next-token
                                         (make-permissive-tokenizer tokenizer all-tokens-hash/mutable))
                                       (THE-GRAMMAR next-token)]
                                      [(source tokenizer)
                                       (parameterize ([current-source source])
                                         (PARSE tokenizer))])
                         (string->symbol (format "~a-rule-parser" 'start-rule)))))]))
             
             (define PARSE (procedure-rename (MAKE-RULE-PARSER start-id) 'PARSE))
             
             (define (PARSE-TO-DATUM x)
               (let loop ([x (syntax->datum (PARSE x))])
                 (cond
                   [(list? x) (map loop x)]
                   [else x])))

             (define PARSE-TREE PARSE-TO-DATUM)))))]))


;; Given a flattened rule, returns a syntax for the code that
;; preserves as much source location as possible.
;;
;; Each rule is defined to return a list with the following structure:
;;
;;     stx :== (name (U tokens rule-stx) ...)
;;
(define (flat-rule->yacc-rule a-flat-rule)
  (syntax-case a-flat-rule ()
    [(rule-type origin name clauses ...)
     (begin
       (define translated-clauses
         (map (lambda (clause) (translate-clause clause #'name #'origin))
              (syntax->list #'(clauses ...))))
       (with-syntax ([(translated-clause ...) translated-clauses])
         #`[name translated-clause ...]))]))



;; translates a single primitive rule clause.
;; A clause is a simple list of ids, lit, vals, and inferred-id elements.
;; The action taken depends on the pattern type.
(define (translate-clause a-clause rule-name/false origin)
  (define translated-patterns
    (let loop ([primitive-patterns (syntax->list a-clause)])
      (cond
        [(empty? primitive-patterns)
         '()]
        [else
         (cons (syntax-case (first primitive-patterns) (id lit token inferred-id)
                 [(id val)
                  #'val]
                 [(lit val)
                  (datum->syntax #f (string->symbol (syntax-e #'val)) #'val)]
                 [(token val)
                  #'val]
                 [(inferred-id val reason)
                  #'val])
               (loop (rest primitive-patterns)))])))
  
  (define translated-actions
    (for/list ([translated-pattern (in-list translated-patterns)]
               [primitive-pattern (syntax->list a-clause)]
               [pos (in-naturals 1)])
      (if (eq? (syntax-property primitive-pattern 'hide) 'hide)
          #'null
          (with-syntax ([$X 
                         (format-id translated-pattern "$~a" pos)]
                        [$X-start-pos 
                         (format-id translated-pattern "$~a-start-pos" pos)]
                        [$X-end-pos
                         (format-id translated-pattern "$~a-end-pos" pos)])
            (syntax-case primitive-pattern (id lit token inferred-id)
                      
              ;; When a rule usage is inferred, the value of $X is a syntax object
              ;; whose head is the name of the inferred rule . We strip that out,
              ;; leaving the residue to be absorbed.
              [(inferred-id val reason)
               #'(syntax-case $X ()
                   [(inferred-rule-name . rest)
                    (syntax->list #'rest)])]
              [(id val)
               ;; at this point, the 'hide property is either #f or "splice"
               ;; ('hide value is handled at the top of this conditional
               ;; we need to use boolean because a symbol is treated as an identifier.
               ;; also we'll separate it into its own property for clarity and test for it in "runtime.rkt"
               #`(list (syntax-property $X 'splice-rh-id #,(and (syntax-property primitive-pattern 'hide) #t)))]
              [(lit val)
               #'(list (atomic-datum->syntax $X $X-start-pos $X-end-pos))]
              [(token val)
               #'(list (atomic-datum->syntax $X $X-start-pos $X-end-pos))])))))
  
  (define whole-rule-loc
    (if (empty? translated-patterns)
        #'(list (current-source) #f #f #f #f)
        (with-syntax ([$1-start-pos (datum->syntax (first translated-patterns) '$1-start-pos)]
                      [$n-end-pos (format-id (last translated-patterns) "$~a-end-pos" (length translated-patterns))])
          #`(positions->srcloc $1-start-pos $n-end-pos))))

  ;; move 'hide-or-splice-lhs-id property into function because name is datum-ized
  (with-syntax ([(translated-pattern ...) translated-patterns]
                [(translated-action ...) translated-actions])
    #`[(translated-pattern ...)
       (rule-components->syntax '#,rule-name/false translated-action ...
                                #:srcloc #,whole-rule-loc
                                #:hide-or-splice? #,(syntax-property rule-name/false 'hide-or-splice-lhs-id))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect-token-types: (listof rule-syntax) -> (values (listof identifier) (listof identifier))
;;
;; Given a rule, automatically derive the list of implicit and
;; explicit token types we need to generate.
;; 
;; Note: EOF is reserved, and will always be included in the list
;; of explicit token types, though the user is not allow to express it themselves.
(define (rules-collect-token-types rules)
  (define-values (implicit explicit)
    (for/fold ([implicit '()]
               [explicit (list (datum->syntax (first rules) 'EOF))])
              ([r (in-list rules)])
      (rule-collect-token-types r implicit explicit)))
  (values (reverse implicit) (reverse explicit)))

(define (rule-collect-token-types a-rule implicit explicit)
  (syntax-case a-rule (rule)
    [(rule id a-pattern)
     (pattern-collect-implicit-token-types #'a-pattern implicit explicit)]))

(define (pattern-collect-implicit-token-types a-pattern implicit explicit)
  (let loop ([a-pattern a-pattern]
             [implicit implicit]
             [explicit explicit])
    (syntax-case a-pattern (id lit token choice repeat maybe seq)
      [(id val)
       (values implicit explicit)]
      [(lit val)
       (values (cons #'val implicit) explicit)]
      [(token val)
       (begin
         (when (eq? (syntax-e #'val) 'EOF)
           (raise-syntax-error #f "Token EOF is reserved and can not be used in a grammar" #'val))
         (values implicit (cons #'val explicit)))]
      [(choice vals ...)
       (for/fold ([implicit implicit]
                  [explicit explicit])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v implicit explicit))]
      [(repeat min max val)
       (loop #'val implicit explicit)]
      [(maybe val)
       (loop #'val implicit explicit)]
      [(seq vals ...)
       (for/fold ([implicit implicit]
                  [explicit explicit])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v implicit explicit))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rule-id: rule -> identifier-stx
;; Get the binding id of a rule.
(define (rule-id a-rule)
  (syntax-case a-rule (rule)
    [(rule id a-pattern)
     #'id]))

(define (rule-pattern a-rule)
  (syntax-case a-rule (rule)
    [(rule id a-pattern)
     #'a-pattern]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-all-rules-defined!: (listof rule-stx) -> void
(define (check-all-rules-defined! rules)
  (define table (make-free-id-table))
  ;; Pass one: collect all the defined rule names.
  (for ([a-rule (in-list rules)])
    (free-id-table-set! table (rule-id a-rule) #t))
  ;; Pass two: check each referenced id, and make sure it's been defined.
  (for ([a-rule (in-list rules)])
    (for ([referenced-id (in-list (rule-collect-used-ids a-rule))])
      (unless (free-id-table-ref table referenced-id (lambda () #f))
        (raise-syntax-error #f (format "Rule ~a has no definition" (syntax-e referenced-id))
                            referenced-id)))))

;; check-all-rules-no-duplicates!: (listof rule-stx) -> void
(define (check-all-rules-no-duplicates! rules)
  (define table (make-free-id-table))
  ;; Pass one: collect all the defined rule names.
  (for ([a-rule (in-list rules)])
    (define maybe-other-rule-id (free-id-table-ref table (rule-id a-rule) (lambda () #f)))
    (when maybe-other-rule-id
      (raise-syntax-error #f (format "Rule ~a has a duplicate definition" (syntax-e (rule-id a-rule)))
                          (rule-id a-rule)
                          #f
                          (list (rule-id a-rule) maybe-other-rule-id)))
    (free-id-table-set! table (rule-id a-rule) (rule-id a-rule))))



;; rule-collect-used-ids: rule-stx -> (listof identifier)
;; Given a rule, extracts a list of identifiers
(define (rule-collect-used-ids a-rule)
  (syntax-case a-rule (rule)
    [(rule id a-pattern)
     (pattern-collect-used-ids #'a-pattern '())]))

;; pattern-collect-used-ids: pattern-stx (listof identifier) -> (listof identifier)
;; Returns a flat list of rule identifiers referenced in the pattern.
(define (pattern-collect-used-ids a-pattern acc)
  (let loop ([a-pattern a-pattern]
             [acc acc])
    (syntax-case a-pattern (id lit token choice repeat maybe seq)
      [(id val)
       (cons #'val acc)]
      [(lit val)
       acc]
      [(token val)
       acc]
      [(choice vals ...)
       (for/fold ([acc acc])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v acc))]
      [(repeat min max val)
       (loop #'val acc)]
      [(maybe val)
       (loop #'val acc)]
      [(seq vals ...)
       (for/fold ([acc acc])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v acc))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-all-rules-satisfiable: (listof rule-stx) -> void
;; Does a simple graph traversal / topological sort-like thing to make sure that, for
;; any rule, there's some finite sequence of tokens that
;; satisfies it.  If this is not the case, then something horrible
;; has happened, and we need to tell the user about it.
;;
;; NOTE: Assumes all referenced rules have definitions.
(define (check-all-rules-satisfiable! rules)
  (define toplevel-rule-table (make-free-id-table))
  (for ([a-rule (in-list rules)])
    (free-id-table-set! toplevel-rule-table 
                        (rule-id a-rule) 
                        (sat:make-and)))
  (define leaves '())
  
  (define (make-leaf)
    (define a-leaf (sat:make-and))
    (set! leaves (cons a-leaf leaves))
    a-leaf)
  
  (define (process-pattern a-pattern)
    (syntax-case a-pattern (id lit token choice repeat maybe seq)
      [(id val)
       (free-id-table-ref toplevel-rule-table #'val)]
      [(lit val)
       (make-leaf)]
      [(token val)
       (make-leaf)]
      [(choice vals ...)
       (begin
         (define an-or-node (sat:make-or))
         (for ([v (in-list (syntax->list #'(vals ...)))])
           (define a-child (process-pattern v))
           (sat:add-child! an-or-node a-child))
         an-or-node)]
      [(repeat min max val)
       (syntax-case #'min ()
         [0
          (make-leaf)]
         [else
          (process-pattern #'val)])]
      [(maybe val)
       (make-leaf)]
      [(seq vals ...)
       (begin
         (define an-and-node (sat:make-and))
         (for ([v (in-list (syntax->list #'(vals ...)))])
           (define a-child (process-pattern v))
           (sat:add-child! an-and-node a-child))
         an-and-node)]))
  
  (for ([a-rule (in-list rules)])
    (define rule-node (free-id-table-ref toplevel-rule-table (rule-id a-rule)))
    (sat:add-child! rule-node (process-pattern (rule-pattern a-rule))))
  
  (for ([a-leaf leaves])
    (sat:visit! a-leaf))
  
  (for ([a-rule (in-list rules)])
    (define rule-node (free-id-table-ref toplevel-rule-table (rule-id a-rule)))
    (unless (sat:node-yes? rule-node)
      (raise-syntax-error #f (format "Rule ~a has no finite derivation" (syntax-e (rule-id a-rule)))
                          (rule-id a-rule)))))
