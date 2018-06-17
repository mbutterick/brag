#lang racket/base
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are just here to provide bindings for Check Syntax.
;; Otherwise, we should never hit these, as the toplevel rules-codegen
;; should eliminate all uses of these if it does the right thing.
(define-syntax-rule (define-errors ID ...)
  (begin (define (ID stx) (raise-syntax-error 'ID "Used out of context of rules" stx)) ...))

(define-errors rules rule id lit token choice repeat maybe seq)