#lang racket/base
(require br-parser-tools/yacc
         br-parser-tools/lex
         racket/list
         racket/match
         "rule-structs.rkt")

;; A parser for grammars.

(provide hide-char
         splice-char
         tokens
         token-LPAREN
         token-RPAREN
         token-HIDE ; for hider
         token-SPLICE ; for splicer
         token-LBRACKET
         token-RBRACKET
         token-PIPE
         token-REPEAT
         token-RULE_HEAD
         token-RULE_HEAD_HIDDEN
         token-RULE_HEAD_SPLICED
         token-ID
         token-LIT
         token-EOF
         token-EMPTY
         grammar-parser
         
         current-source
         current-parser-error-handler
         current-prefix-out
         
         [struct-out rule]
         [struct-out lhs-id]
         [struct-out pattern]
         [struct-out pattern-id]
         [struct-out pattern-lit]
         [struct-out pattern-token]
         [struct-out pattern-choice]
         [struct-out pattern-repeat]
         [struct-out pattern-seq])

(define-tokens tokens (LPAREN
                       RPAREN
                       LBRACKET
                       RBRACKET
                       HIDE
                       SPLICE
                       PIPE
                       REPEAT
                       RULE_HEAD
                       RULE_HEAD_HIDDEN
                       RULE_HEAD_SPLICED
                       ID
                       LIT
                       EOF
                       EMPTY))

(define hide-char #\/)
(define splice-char #\@)

(define id-separators-pattern "(:|::=)")

;; grammar-parser: (-> token) -> (listof rule)
(define grammar-parser
  (parser
   (tokens tokens)
   (src-pos)
   (start rules)
   (end EOF)
   
   (grammar
    [rules
     [(rules*) $1]]
    
    [rules*
     [(rule rules*)
      (cons $1 $2)]
     [()
      '()]]
    
    ;; I have a separate token type for rule identifiers to avoid the
    ;; shift/reduce conflict that happens with the implicit sequencing
    ;; of top-level rules.  i.e. the parser can't currently tell, when
    ;; it sees an ID, if it should shift or reduce to a new rule.
    [rule
     [(RULE_HEAD pattern)
      (begin 
        (define trimmed (regexp-replace (pregexp (format "\\s*~a$" id-separators-pattern)) $1 ""))
        (rule (position->pos $1-start-pos)
              (position->pos $2-end-pos)
              (lhs-id (position->pos $1-start-pos)
                      (pos (+ (position-offset $1-start-pos)
                              (string-length trimmed))
                           (position-line $1-start-pos)
                           (position-col $1-start-pos))
                      trimmed
                      #f)
              $2))]
     
     [(RULE_HEAD_HIDDEN pattern) ; slash indicates hiding
      (begin
        (define trimmed (cadr (regexp-match (pregexp (format "~a(\\S+)\\s*~a$" hide-char id-separators-pattern)) $1)))
        (rule (position->pos $1-start-pos)
              (position->pos $2-end-pos)
              (lhs-id (position->pos $1-start-pos)
                      (pos (+ (position-offset $1-start-pos)
                              (string-length trimmed)
                              (string-length "!"))
                           (position-line $1-start-pos)
                           (position-col $1-start-pos))
                      trimmed
                      ''hide) ; symbol needs to be double quoted in this case
              $2))]
     
     [(RULE_HEAD_SPLICED pattern) ; atsign indicates splicing
      (begin
        (define trimmed (cadr (regexp-match (pregexp (format "~a(\\S+)\\s*~a$" splice-char id-separators-pattern)) $1)))
        (rule (position->pos $1-start-pos)
              (position->pos $2-end-pos)
              (lhs-id (position->pos $1-start-pos)
                      (pos (+ (position-offset $1-start-pos)
                              (string-length trimmed)
                              (string-length "@"))
                           (position-line $1-start-pos)
                           (position-col $1-start-pos))
                      trimmed
                      ''splice) ; symbol needs to be double quoted in this case 
              $2))]]
    
    [pattern
     [(implicit-pattern-sequence PIPE pattern)
      (if (pattern-choice? $3)
          (pattern-choice (position->pos $1-start-pos)
                          (position->pos $3-end-pos)
                          (cons $1 (pattern-choice-vals $3))
                          #f)
          (pattern-choice (position->pos $1-start-pos)
                          (position->pos $3-end-pos)
                          (list $1 $3)
                          #f))]
     [(implicit-pattern-sequence)
      $1]]
    
    [implicit-pattern-sequence
     [(repeatable-pattern implicit-pattern-sequence)
      (if (pattern-seq? $2)
          (pattern-seq (position->pos $1-start-pos)
                       (position->pos $2-end-pos)
                       (cons $1 (pattern-seq-vals $2))
                       #f)
          (pattern-seq (position->pos $1-start-pos)
                       (position->pos $2-end-pos)
                       (list $1 $2)
                       #f))]
     [(repeatable-pattern)
      $1]]
    
    [repeatable-pattern
     [(atomic-pattern REPEAT)
      (let ()
        (match-define (cons min-repeat max-repeat)
          (cond [(string=? $2 "*") (cons 0 #f)]
                [(string=? $2 "+") (cons 1 #f)]
                [(string=? $2 "?") (cons 0 1)]
                [(regexp-match #px"^\\{(\\d+)?(,)?(\\d+)?\\}$" $2) ; "{min,max}" with both min & max optional
                 => (λ (m)
                      (match m
                        [(list all min range? max) (let* ([min (if min (string->number min) 0)]
                                                          [max (cond
                                                                 [(and range? max) (string->number max)]
                                                                 [(and (not range?) (not max)) (if (zero? min)
                                                                                                   #f ; {} -> {0,}
                                                                                                   min)] ; {3} -> {3,3}
                                                                 [else #f])])
                                                     (cons min max))]))]
                [else (raise-argument-error 'grammar-parse "unknown repetition operator" $2)]))
        (pattern-repeat (position->pos $1-start-pos)
                        (position->pos $2-end-pos)
                        min-repeat max-repeat $1
                        #f))]
     [(atomic-pattern)
      $1]]
    
    [atomic-pattern
     [(LIT)
      (pattern-lit (position->pos $1-start-pos)
                   (position->pos $1-end-pos)
                   (substring $1 1 (sub1 (string-length $1)))
                   #f)]
     
     [(ID)
      (if (token-id? $1)
          (pattern-token (position->pos $1-start-pos)
                         (position->pos $1-end-pos)
                         $1
                         #f)
          (pattern-id (position->pos $1-start-pos)
                      (position->pos $1-end-pos)
                      $1
                      #f))]

     [(EMPTY)
      (pattern-repeat (position->pos $1-start-pos)
                      (position->pos $1-end-pos)
                      0 0 (pattern-lit (position->pos $1-start-pos)
                                       (position->pos $1-end-pos)
                                       "" #f)
                      #f)]
     
     [(LBRACKET pattern RBRACKET)
      (pattern-repeat (position->pos $1-start-pos)
                      (position->pos $3-end-pos)
                      0 1 $2
                      #f)]
     
     [(LPAREN pattern RPAREN)
      (relocate-pattern $2 (position->pos $1-start-pos) (position->pos $3-end-pos))]
     
     [(HIDE atomic-pattern)
      (relocate-pattern $2 (position->pos $1-start-pos) (position->pos $2-end-pos) 'hide)]

     [(SPLICE ID)
      ;; only works for nonterminals on the right side
      ;; (meaningless with terminals)
      (if (token-id? $2)
          (error 'brag "Can't use splice operator with terminal")
          (pattern-id (position->pos $1-start-pos)
                      (position->pos $2-end-pos)
                      $2
                      'splice))]])
   
   
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ((current-parser-error-handler) tok-ok? tok-name tok-value (position->pos start-pos) (position->pos end-pos))))))

;; relocate-pattern: pattern -> pattern
;; Rewrites the pattern's start and end pos accordingly.
(define (relocate-pattern a-pat start-pos end-pos [hide? #f])
  (match a-pat
    [(pattern-id _ _ v h)
     (pattern-id start-pos end-pos v (or hide? h))]
    [(pattern-token _ _ v h)
     (pattern-token start-pos end-pos v (or hide? h))]
    [(pattern-lit _ _ v h)
     (pattern-lit start-pos end-pos v (or hide? h))]
    [(pattern-choice _ _ vs h)
     (pattern-choice start-pos end-pos vs (or hide? h))]
    [(pattern-repeat _ _ min max v h)
     (pattern-repeat start-pos end-pos min max v (or hide? h))]
    [(pattern-seq _ _ vs h)
     (pattern-seq start-pos end-pos vs (or hide? h))]
    [else
     (error 'relocate-pattern "Internal error when relocating ~s\n" a-pat)]))


; token-id: string -> boolean
;; Produces true if the id we see should be treated as the name of a token.
;; By convention, tokens are all upper-cased.
(define (token-id? id)
  (string=? (string-upcase id)
            id))



;; position->pos: position -> pos
;; Coerses position structures from br-parser-tools/lex to our own pos structures.
(define (position->pos a-pos)
  (pos (position-offset a-pos)
       (position-line a-pos)
       (position-col a-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; During parsing, we should define the source of the input.
(define current-source (make-parameter #f))

(define current-prefix-out (make-parameter #f))


;; When bad things happen, we need to emit errors with source location.
(struct exn:fail:parse-grammar exn:fail (srclocs)
  #:transparent
  #:property prop:exn:srclocs (lambda (instance)
                                (exn:fail:parse-grammar-srclocs instance)))

(define current-parser-error-handler
  (make-parameter
   (lambda (tok-ok? tok-name tok-value start-pos end-pos)
     (raise (exn:fail:parse-grammar
             (format "Error while parsing grammar near: ~e [line=~a, column=~a, position=~a]"
                     tok-value
                     (pos-line start-pos)
                     (pos-col start-pos)
                     (pos-offset start-pos))
             (current-continuation-marks)
             (list (srcloc (current-source)
                           (pos-line start-pos)
                           (pos-col start-pos)
                           (pos-offset start-pos)
                           (if (and (number? (pos-offset end-pos))
                                    (number? (pos-offset start-pos)))
                               (- (pos-offset end-pos)
                                  (pos-offset start-pos))
                               #f))))))))
