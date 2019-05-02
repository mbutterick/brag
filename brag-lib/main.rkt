#lang racket/base

(module+ reader
  (require "codegen/reader.rkt")
  (provide (all-from-out "codegen/reader.rkt")))

;; this creates dummy identifiers
;; so cross-refs in `brag` docs will work
(module names racket/base
  (provide (all-defined-out))
  (define (parse) (void))
  (define (parse-to-datum) (void))
  (define (make-rule-parser) (void))
  (define (all-token-types) (void)))

(require (for-label 'names))
(provide (for-label (all-from-out 'names)))