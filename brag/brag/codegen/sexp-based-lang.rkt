#lang racket/base

;; A language level for automatically generating parsers out of BNF grammars.
;;
;; Danny Yoo (dyoo@hashcollision.org)
;;
;; Intent: make it trivial to generate languages for Racket.  At the
;; moment, I find it painful to use br-parser-tools.  This library is
;; meant to make it less agonizing.
;;
;; The intended use of this language is as follows:
;;
;;;;; s-exp-grammar.rkt ;;;;;;;;;
;; #lang brag
;; s-exp : "(" s-exp* ")" | ATOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; What this generates is:
;;
;;     * parse: a function that consumes a source and a
;;       position-aware lexer, and produces a syntax object.
;;
;;     * make-rule-parser: a custom parser given a provided start rule.
;; 
;; You'll still need to do a little work, by providing a lexer that
;; defines what the uppercased tokens mean.  For example, you can
;; use the br-parser-tools/lex lexer tools:
;;
;; (require brag/support
;;          br-parser-tools/lex
;;          br-parser-tools/lex-sre)
;;
;; (define tokenize
;;   (lexer-src-pos
;;     [(:+ alphabetic)
;;      (token 'ATOM lexeme)]
;;     [whitespace
;;      (return-without-pos (tokenize/1 input-port))]
;;     [(:or "(" ")")
;;      (token lexeme lexeme)]))
;;

;; However, that should be all you need.  The output of an
;; generated grammar is an honest-to-goodness syntax
;; object with source locations, fully-labeled by the rules.
;;
;; (parse (tokenize an-input-port))
;;
;;

;; The first rule is treated as the start rule; any successful parse
;; must finish with end-of-file.


;; Terminology:
;;


;; A rule is a rule identifier, followed by a colon ":", followed by a
;; pattern.

;; A rule identifier is an identifier that is not in upper case.
;; A rule identifier should follow the Racket rules for identifiers,
;; except that it can't contain * or +.
;;
;; A token is a rule identifier that is all in upper case.



;; A pattern may either be 
;;
;;   * an implicit sequence of patterns,
;;
;;   * a literal string,
;;
;;   * a rule identifier,
;;
;;   * a quanitifed pattern, either with "*" or "+",
;;
;;   * an optional pattern: a pattern surrounded by "[" and "]", or
;;
;;   * a grouped sequence: a pattern surrounded by "(" and ")".


(require (for-syntax racket/base
                     "codegen.rkt"
                     syntax/strip-context))

(provide rules 
         (rename-out [my-module-begin #%module-begin])
         #%top-interaction #%top #%app #%datum)


(define-syntax (my-module-begin module-stx)
  (syntax-case module-stx ()
    [(_ RULES-STX)
     (with-syntax ([RULES-STX (for/fold ([stx #'RULES-STX])
                                        ([id (in-list '(parse parse-to-datum parse-tree make-rule-parser all-token-types))])
                                (syntax-property stx id (syntax-local-introduce (replace-context module-stx (datum->syntax #f id)))))])
       #`(#%module-begin
          RULES-STX
          (provide (all-defined-out))))]))


(define-syntax (rules rules-stx)
  (rules-codegen #:parser-provider-module 'brag/cfg-parser/cfg-parser ;; 'br-parser-tools/yacc 
                 #:parser-provider-form   'cfg-parser                 ;; 'parser
                 rules-stx))
