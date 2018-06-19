#lang racket/base
(require brag/examples/hide-top
         brag/support
         rackunit)

;; check that the top rule name can be cut (hidden)

(check-equal? (parse-to-datum "x") '("x"))