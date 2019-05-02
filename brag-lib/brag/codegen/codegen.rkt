#lang racket/base
(require racket/list
         racket/syntax
         brag/rules/stx-types
         syntax/id-table
         (prefix-in sat: "satisfaction.rkt")
         (for-template racket/base
                       brag/codegen/runtime
                       brag/private/internal-support))

(provide (all-defined-out)
         (for-template (all-from-out brag/codegen/runtime
                                     brag/private/internal-support)))

;; Given a flattened rule, returns a syntax for the code that
;; preserves as much source location as possible.
;;
;; Each rule is defined to return a list with the following structure:
;;
;;     stx :== (name (U tokens rule-stx) ...)
;;
(define (flat-rule->yacc-rule a-flat-rule)
  (syntax-case a-flat-rule ()
    [(rule-type origin name . clauses)
     (with-syntax ([translated-clauses (for/list ([clause-stx (in-list (syntax->list #'clauses))])
                                         (translate-clause clause-stx #'name #'origin))])
       #'[name . translated-clauses])]))



;; translates a single primitive rule clause.
;; A clause is a simple list of ids, lit, vals, and inferred-id elements.
;; The action taken depends on the pattern type.
(define (translate-clause a-clause rule-name/false origin)
  (define translated-patterns
    (let loop ([primitive-patterns (syntax->list a-clause)])
      (cond
        [(empty? primitive-patterns) null]
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
               [primitive-pattern (in-list (syntax->list a-clause))]
               [pos (in-naturals 1)])
      (if (eq? (syntax-property primitive-pattern 'hide) 'hide)
          #'null
          (with-syntax ([$X (format-id translated-pattern "$~a" pos)]
                        [$X-start-pos (format-id translated-pattern "$~a-start-pos" pos)]
                        [$X-end-pos (format-id translated-pattern "$~a-end-pos" pos)])
            (syntax-case primitive-pattern (id lit token inferred-id)
                      
              ;; When a rule usage is inferred, the value of $X is a syntax object
              ;; whose head is the name of the inferred rule. We strip that out,
              ;; leaving the residue to be absorbed.
              [(inferred-id val reason)
               #'(syntax-case $X ()
                   [(inferred-rule-name . rest)
                    (syntax->list #'rest)])]
              [(id val)
               ;; at this point, the 'hide property is either #f or "splice"
               ;; ('hide value is handled at the top of this conditional)
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
    (for/fold ([implicit null]
               [explicit (list (datum->syntax (first rules) 'EOF))])
              ([a-rule (in-list rules)])
      (syntax-case a-rule (rule)
        [(rule _ a-pattern)
         (let loop ([a-pattern #'a-pattern]
                    [implicit implicit]
                    [explicit explicit])
           (syntax-case a-pattern (id lit token choice repeat maybe seq EOF)
             [(id val)
              (values implicit explicit)]
             [(lit val)
              (values (cons #'val implicit) explicit)]
             [(token EOF)
              (raise-syntax-error #f "Token EOF is reserved and can not be used in a grammar" #'val)]
             [(token val)
              (values implicit (cons #'val explicit))]
             [(choice . vals)
              (for/fold ([implicit implicit]
                         [explicit explicit])
                        ([v (in-list (syntax->list #'vals))])
                (loop v implicit explicit))]
             [(repeat min max val)
              (loop #'val implicit explicit)]
             [(maybe val)
              (loop #'val implicit explicit)]
             [(seq . vals)
              (for/fold ([implicit implicit]
                         [explicit explicit])
                        ([v (in-list (syntax->list #'vals))])
                (loop v implicit explicit))]))])))
  (values (reverse implicit) (reverse explicit)))


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
  (for* ([a-rule (in-list rules)]
         [referenced-id (in-list (rule-collect-used-ids a-rule))]
         #:unless (free-id-table-ref table referenced-id (λ () #f)))
    (raise-syntax-error #f (format "Rule ~a has no definition" (syntax-e referenced-id))
                        referenced-id)))

;; check-all-rules-no-duplicates!: (listof rule-stx) -> void
(define (check-all-rules-no-duplicates! rules)
  (define table (make-free-id-table))
  ;; Pass one: collect all the defined rule names.
  (for ([a-rule (in-list rules)])
    (define maybe-other-rule-id (free-id-table-ref table (rule-id a-rule) (λ () #f)))
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
     (pattern-collect-used-ids #'a-pattern null)]))

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
      [(choice . vals)
       (for/fold ([acc acc])
                 ([v (in-list (syntax->list #'vals))])
         (loop v acc))]
      [(repeat min max val)
       (loop #'val acc)]
      [(maybe val)
       (loop #'val acc)]
      [(seq . vals)
       (for/fold ([acc acc])
                 ([v (in-list (syntax->list #'vals))])
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
  (define toplevel-rule-table
    (make-free-id-table (for/list ([a-rule (in-list rules)])
                          (cons (rule-id a-rule) (sat:make-and)))))
  
  (define leaves null)
  
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
      [(choice . vals)
       (let ([an-or-node (sat:make-or)])
         (for* ([v (in-list (syntax->list #'vals))]
                [a-child (in-value (process-pattern v))])
           (sat:add-child! an-or-node a-child))
         an-or-node)]
      [(repeat min max val)
       (syntax-case #'min ()
         [0 (make-leaf)]
         [_ (process-pattern #'val)])]
      [(maybe val) (make-leaf)]
      [(seq . vals)
       (let ([an-and-node (sat:make-and)])
         (for* ([v (in-list (syntax->list #'vals))]
                [a-child (in-value (process-pattern v))])
           (sat:add-child! an-and-node a-child))
         an-and-node)]))
  
  (for* ([a-rule (in-list rules)]
         [rule-node (in-value (free-id-table-ref toplevel-rule-table (rule-id a-rule)))])
    (sat:add-child! rule-node (process-pattern (rule-pattern a-rule))))
  
  (for-each sat:visit! leaves)
  
  (for* ([a-rule (in-list rules)]
         [rule-node (in-value (free-id-table-ref toplevel-rule-table (rule-id a-rule)))]
         #:unless (sat:node-yes? rule-node))
    (raise-syntax-error #f
                        (format "Rule ~a has no finite derivation" (syntax-e (rule-id a-rule)))
                        (rule-id a-rule))))