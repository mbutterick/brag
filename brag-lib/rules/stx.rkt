#lang racket/base

(require "rule-structs.rkt"
         br-parser-tools/lex
         racket/match
         syntax/strip-context)

(provide rule->stx)

(define (rule->stx source a-rule)
  (define id-stx
    (syntax-property
     (datum->syntax #f
                    (string->symbol (lhs-id-val (rule-lhs a-rule)))
                    (list source
                          (pos-line (lhs-id-start (rule-lhs a-rule)))
                          (pos-col (lhs-id-start (rule-lhs a-rule)))
                          (pos-offset (lhs-id-start (rule-lhs a-rule)))
                          (if (and (number? (pos-offset (lhs-id-start (rule-lhs a-rule))))
                                   (number? (pos-offset (lhs-id-end (rule-lhs a-rule)))))
                              (- (pos-offset (lhs-id-end (rule-lhs a-rule)))
                                 (pos-offset (lhs-id-start (rule-lhs a-rule))))
                              #f)))
     'hide-or-splice-lhs-id (lhs-id-splice (rule-lhs a-rule))))
  (define pattern-stx (pattern->stx source (rule-pattern a-rule)))
  (define line (pos-line (rule-start a-rule)))
  (define column (pos-col (rule-start a-rule)))
  (define position (pos-offset (rule-start a-rule)))
  (define span (if (and (number? (pos-offset (rule-start a-rule)))
                        (number? (pos-offset (rule-end a-rule))))
                   (- (pos-offset (rule-end a-rule))
                      (pos-offset (rule-start a-rule)))
                   #f))
  (datum->syntax #f
                 `(rule ,id-stx ,pattern-stx)
                 (list source line column position span)))


(define (pattern->stx source a-pattern)
  
  (define (pat->srcloc source pat)
    (match-define (pos offset line col) (pattern-start pat))
    (define offset-end (pos-offset (pattern-end pat)))
    (define span (and (number? offset) (number? offset-end) (- offset-end offset)))
    (list source line col offset span))

  (let loop ([a-pattern a-pattern] [hide-state #f]) 
    (define (pat->stx val) (datum->syntax #f val (pat->srcloc source a-pattern))) 
    (define-values (pat hide)
      (match a-pattern
        [(struct pattern-id (start end val hide)) (values `(id ,(pat->stx (string->symbol val))) hide)]
        [(struct pattern-lit (start end val hide)) (values `(lit ,(pat->stx val)) hide)]
        [(struct pattern-token (start end val hide)) (values `(token ,(pat->stx (string->symbol val))) hide)]
        ;; propagate hide value of choice, repeat, and seq into subpatterns
        ;; use `(or hide-state hide)` to capture parent value
        [(struct pattern-choice (start end vals hide))
         (values `(choice ,@(map (λ (val) (loop val (or hide-state hide))) vals)) hide)]
        [(struct pattern-repeat (start end min max val hide))
         (values `(repeat ,min ,max ,(loop val (or hide-state hide))) hide)]
        [(struct pattern-seq (start end vals hide))
         (values `(seq ,@(map (λ (val) (loop val (or hide-state hide))) vals)) hide)]))
    
    (syntax-property (pat->stx pat) 'hide (or hide-state hide))))
