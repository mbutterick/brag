#lang racket/base
(require (for-syntax racket/base "codegen.rkt"))
(provide (all-from-out racket/base)) ; borrow #%module-begin from racket/base

;; body of module invokes `rules`
(provide rules)
(define-syntax (rules rules-stx) (rules-codegen rules-stx))
