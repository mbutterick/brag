#lang racket/base
(require racket/match 
         racket/list
         racket/generator
         (prefix-in lex: br-parser-tools/lex) 
         brag/support
         brag/private/internal-support)


(provide the-error-handler 
         make-permissive-tokenizer
         atomic-datum->syntax
         positions->srcloc
         rule-components->syntax
         remove-rule-name)



;; The level of indirection here is necessary since the yacc grammar wants a
;; function value for the error handler up front.  We want to delay that decision
;; till parse time.
(define (the-error-handler tok-ok? tok-name tok-value start-pos end-pos)
  (match (positions->srcloc start-pos end-pos)
    [(list src line col offset span)
     ((current-parser-error-handler) tok-name 
                                     tok-value
                                     offset
                                     line
                                     col 
                                     span)]))




(define no-position (lex:position #f #f #f))
(define (no-position? p)
  (not 
   (or (lex:position-line p)
       (lex:position-col p)
       (lex:position-offset p))))


;; make-permissive-tokenizer: (U (sequenceof (U token token-struct eof void)) (-> (U token token-struct eof void))) hash -> (-> position-token)
;; Creates a tokenizer from the given value.
;; FIXME: clean up code.
(define (make-permissive-tokenizer tokenizer token-type-hash)
  (define tokenizer-thunk (cond
                            [(sequence? tokenizer)
                             (sequence->generator tokenizer)]
                            [(procedure? tokenizer)
                             tokenizer]))
  
  ;; lookup: symbol any pos pos -> position-token
  (define (lookup type val start-pos end-pos)
    (lex:position-token
     ((hash-ref token-type-hash type
                (lambda ()
                  ((current-tokenizer-error-handler) (format "~a" type) val
                                                     (lex:position-offset start-pos)
                                                     (lex:position-line start-pos)
                                                     (lex:position-col start-pos)
                                                     (and (number? (lex:position-offset start-pos))
                                                          (number? (lex:position-offset end-pos))
                                                          (- (lex:position-offset end-pos) 
                                                             (lex:position-offset start-pos))))))
      val)
     start-pos end-pos))
  
  (define (permissive-tokenizer)
    (define next-token (tokenizer-thunk))
    (let loop ([next-token next-token][start no-position][end no-position])
      (match next-token
        [(or (? eof-object?) (? void?))
         (lookup 'EOF eof start end)]
        
        [(? symbol?)
         (lookup next-token next-token start end)]
        
        [(or (? string?) (? char?))
         (define next-token-str (format "~a" next-token))
         (lookup (string->symbol next-token-str) next-token-str start end)]
        
        ;; Compatibility 
        [(? lex:token?)
         (loop (token (lex:token-name next-token)
                      (lex:token-value next-token)) start end)]
        
        [(token-struct type val offset line column span skip?)
         (cond [skip?
                ;; skip whitespace, and just tokenize again.
                (permissive-tokenizer)]
               
               [(hash-has-key? token-type-hash type)
                (define start-pos (lex:position offset line column))
                ;; try to synthesize a consistent end position.
                (define end-pos (lex:position (if (and (number? offset) (number? span))
                                                  (+ offset span)
                                                  offset)
                                              line
                                              (if (and (number? column) (number? span))
                                                  (+ column span)
                                                  column)))
                (lookup type val start-pos end-pos)]
               [else
                ;; We ran into a token of unrecognized type.  Let's raise an appropriate error.
                ((current-tokenizer-error-handler) type val 
                                                   offset line column span)])]
        
        ;; for the next two cases:
        ;; carry the token's start and end position into the `a-position-token` recursion
        ;; so that if an error arises, it's reported as coming from the location of the containing token
        [(lex:position-token t s e)
         (define a-position-token (loop t s e))
         (lex:position-token (lex:position-token-token a-position-token)
                             (if (no-position? (lex:position-token-start-pos a-position-token))
                                 s
                                 (lex:position-token-start-pos a-position-token))
                             (if (no-position? (lex:position-token-end-pos a-position-token))
                                 e
                                 (lex:position-token-end-pos a-position-token)))]

        [(lex:srcloc-token t loc)
         (define s (lex:position (srcloc-position loc) (srcloc-line loc) (srcloc-column loc)))
         (define e (lex:position (+ (srcloc-position loc) (srcloc-span loc)) #f #f))
         (define a-position-token (loop t s e))
         (lex:position-token (lex:position-token-token a-position-token)
                             (if (no-position? (lex:position-token-start-pos a-position-token))
                                 s
                                 (lex:position-token-start-pos a-position-token))
                             (if (no-position? (lex:position-token-start-pos a-position-token))
                                 e
                                 (lex:position-token-end-pos a-position-token)))]
        
        [else
         ;; Otherwise, we have no idea how to treat this as a token.
         ((current-tokenizer-error-handler) 'unknown-type (format "~a" next-token)
                                            #f #f #f #f)])))
  permissive-tokenizer)



;; positions->srcloc: position position -> (list source line column offset span)
;; Given two positions, returns a srcloc-like structure, where srcloc is the value
;; consumed as the third argument to datum->syntax.
(define (positions->srcloc start-pos end-pos)
  (list (current-source)
        (lex:position-line start-pos)
        (lex:position-col start-pos)
        (lex:position-offset start-pos)
        (if (and (number? (lex:position-offset end-pos))
                 (number? (lex:position-offset start-pos)))
            (- (lex:position-offset end-pos)
               (lex:position-offset start-pos))
            #f)))

#|
MB: the next three functions control the parse tree output.
This would be the place to check a syntax property for hiding.
|#
;; We create a syntax using read-syntax; by definition, it should have the
;; original? property set to #t, which we then copy over to syntaxes constructed
;; with atomic-datum->syntax and rule-components->syntax.
(define stx-with-original?-property
  (read-syntax #f (open-input-string "meaningless-string")))


;; atomic-datum->syntax: datum position position
;; Helper that does the ugly work in wrapping a datum into a syntax
;; with source location.
(define (atomic-datum->syntax d start-pos end-pos)
  (datum->syntax #f d (positions->srcloc start-pos end-pos) stx-with-original?-property))


(define (remove-rule-name component-stx #:splice? [splice #f])
  ;; when removing a rule name, we apply it as a syntax property to the remaining elements
  ;; for possible later usage (aka, why throw away information)   
  (syntax-case component-stx ()
    [(name . subcomponents)
     (let ([name-datum (syntax->datum #'name)])
       ;; two properties: 'rule returns name-datum, and name-datum returns original #'name stx
       (define (annotate-name stx) (syntax-property (syntax-property stx name-datum #'name) 'rule name-datum))
       (if splice
           ;; when splicing, returned list is a regular list, with each element having the property.
           (map annotate-name (syntax->list #'subcomponents))
           ;; when hiding, returned list should be a syntaxed list with the property
           ;; iow, basically the same as `component-stx`, minus the name
           (annotate-name (datum->syntax component-stx #'subcomponents component-stx component-stx))))]
    [_ (raise-syntax-error 'remove-rule-name "component has no name" component-stx)]))


(define (preprocess-component component-stx)
  (cond
    ;; test splice first in case both hiding and splicing are set, for instance:
    ;; /rule : thing @rule
    ;; otherwise the hide prevents the splice from being expressed
    [(or (eq? (syntax-property component-stx 'hide-or-splice) 'splice)
         (syntax-property component-stx 'splice-rh-id))
     (remove-rule-name component-stx #:splice? #t)] ; spliced version is lifted out of the sublist
    [(eq? (syntax-property component-stx 'hide-or-splice) 'hide)
     (list (remove-rule-name component-stx))] ; hidden version still wrapped in a sublist
    [else (list component-stx)]))


(define (preprocess-component-lists component-stxss)
  ; "preprocess" means splicing and rule-name-hiding where indicated
  ;; inside `component-stx` is a rule name followed by subcomponents
  (append*
   (for*/list ([component-stxs (in-list component-stxss)]
               [component-stx (in-list component-stxs)])
              (preprocess-component component-stx))))


;; rule-components->syntax: (U symbol false) (listof stx) ... #:srcloc (U #f (list src line column offset span)) -> stx
;; Creates an stx out of the rule name and its components.
;; The location information of the rule spans that of its components.
(define (rule-components->syntax rule-name/false #:srcloc [srcloc #f] #:hide-or-splice? [hide-or-splice #f] . component-lists)
  (define new-rule-name
    ;; stash the hide property on rule names so we can use it later if we want
    (syntax-property (datum->syntax #f rule-name/false srcloc stx-with-original?-property) 'hide-or-splice? hide-or-splice))
  (define new-rule-components (preprocess-component-lists component-lists))
  (define rule-result (cons new-rule-name new-rule-components))
  (define syntaxed-rule-result (datum->syntax #f rule-result srcloc stx-with-original?-property))
  ;; not 'hide-or-splice-lhs-id, because this will now become a (right-hand) component in a different (left-hand) rule
  ;; actual splicing happens when the parent rule is processed (with procedure above)
  (syntax-property syntaxed-rule-result 'hide-or-splice hide-or-splice))

